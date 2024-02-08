//
// Created by Avently on 09.02.2023.
// Copyright (c) 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import WebRTC
import SimpleXChat
import AVFoundation
import AVKit

struct CallUIView: View {
    @ObservedObject var call: Call
    @Binding var activeCall: WebRTCClient.Call?
    var client: WebRTCClient
    @Binding var activeCallViewIsCollapsed: Bool
    @Binding var localRendererAspectRatio: CGFloat?

    var body: some View {
        ZStack(alignment: .bottom) {
            if [call.peerMedia, call.localMedia].contains(.video), activeCall != nil {
                GeometryReader { g in
                    let width = g.size.width * 0.3

                    ZStack(alignment: .topTrailing) {
                        CallViewRemote(client: client, activeCall: $activeCall)
                        CallViewLocal(client: client, activeCall: $activeCall, localRendererAspectRatio: $localRendererAspectRatio)
                            .cornerRadius(10)
                            .frame(width: width, height: width / (localRendererAspectRatio ?? 1))
                            .padding([.top, .trailing], 17)
                    }.onAppear {
                        activeCallViewIsCollapsed = true
                    }
                }
            }
            if let call = ChatModel.shared.activeCall {
                ActiveCallOverlay(call: call, client: client)
            }
        }
    }
}

struct CallUI: UIViewControllerRepresentable {
    @ObservedObject var call: Call
    @Binding var activeCall: WebRTCClient.Call?
    var client: WebRTCClient
    @Binding var activeCallViewIsCollapsed: Bool
    @Binding var localRendererAspectRatio: CGFloat?
    @State var pipShown: Bool = false
    @State var enablePip: (Bool) -> Void = {_ in }
    @State var pipInited: Bool = false

    func makeUIViewController(context: Context) -> UIHostingController<CallUIView> {
        let controller = UIHostingController(rootView: CallUIView(call: call, activeCall: $activeCall, client: client, activeCallViewIsCollapsed: $activeCallViewIsCollapsed, localRendererAspectRatio: $localRendererAspectRatio))
        return controller
    }

    func updateUIViewController(_ uiViewController: UIHostingController<CallUIView>, context: Context) {
        if activeCallViewIsCollapsed != pipShown {
            enablePip(activeCallViewIsCollapsed)
        }
        if !pipInited, let call = activeCall, AVPictureInPictureController.isPictureInPictureSupported() {
            DispatchQueue.main.async {
                pipInited = true
            }
            makeViewWithRTCRenderer(call, uiViewController.view, context)
            //makeViewWithFrameRenderer(call, view, context)
        }
    }

    func makeCoordinator() -> Coordinator {
        Coordinator()
    }

    func makeViewWithRTCRenderer(_ call: WebRTCClient.Call/*, _ remoteRenderer: RTCMTLVideoView*/, _ view: UIView, _ context: Context) {
        let pipRemoteRenderer = RTCMTLVideoView(frame: view.frame)
        pipRemoteRenderer.videoContentMode = .scaleAspectFill

        let pipVideoCallViewController = AVPictureInPictureVideoCallViewController()
        pipVideoCallViewController.preferredContentSize = CGSize(width: 1080, height: 1920)
        addSubviewAndResize(pipRemoteRenderer, into: pipVideoCallViewController.view)
        let pipContentSource = AVPictureInPictureController.ContentSource(
            activeVideoCallSourceView: view,
            contentViewController: pipVideoCallViewController
        )

        let pipController = AVPictureInPictureController(contentSource: pipContentSource)
        pipController.canStartPictureInPictureAutomaticallyFromInline = true
        pipController.delegate = context.coordinator
        //client.addRemoteRenderer(call, pipRemoteRenderer)
        context.coordinator.willShowHide = { show in
            DispatchQueue.main.async {
                if show {
                    client.addRemoteRenderer(call, pipRemoteRenderer)
                    //remoteRenderer.isHidden = true
                    activeCallViewIsCollapsed = true
                } else {
                    activeCallViewIsCollapsed = false
                }
            }
        }
        context.coordinator.didShowHide = { show in
            DispatchQueue.main.async {
                if show {
                    //
                } else {
                    client.removeRemoteRenderer(call, pipRemoteRenderer)
                    //remoteRenderer.isHidden = false
                }
                pipShown = show
            }
        }
        DispatchQueue.main.async {
            enablePip = { enable in
                if enable != pipShown /* pipController.isPictureInPictureActive */ {
                    DispatchQueue.main.async {
                        if enable {
                            pipController.startPictureInPicture()
                            logger.debug("LALAL PIP TRY START")
                        } else {
                            pipController.stopPictureInPicture()
                        }
                        pipShown = enable
                    }
                }
            }
        }
        logger.debug("LALAL PIP possible: \(pipController.isPictureInPicturePossible)")
    }

    // MARK: - Coordinator
    class Coordinator: NSObject, AVPictureInPictureControllerDelegate {
        var willShowHide: (Bool) -> Void = { _ in }
        var didShowHide: (Bool) -> Void = { _ in }

        func pictureInPictureControllerWillStartPictureInPicture(_ pictureInPictureController: AVPictureInPictureController) {
            logger.error("LALAL PIP WILL START")
            willShowHide(true)
        }

        func pictureInPictureControllerDidStartPictureInPicture(_ pictureInPictureController: AVPictureInPictureController) {
            didShowHide(true)
            logger.error("LALAL PIP DID START \(pictureInPictureController.isPictureInPictureActive) \(pictureInPictureController.isPictureInPicturePossible) \(pictureInPictureController.isPictureInPictureSuspended) \(pictureInPictureController.debugDescription)")
        }

        func pictureInPictureController(_ pictureInPictureController: AVPictureInPictureController, failedToStartPictureInPictureWithError error: Error) {
            logger.error("LALAL PIP FAILED TO START \(error.localizedDescription)")
        }

        func pictureInPictureControllerWillStopPictureInPicture(_ pictureInPictureController: AVPictureInPictureController) {
            logger.error("LALAL PIP WILL STOP")
            willShowHide(false)
        }

        func pictureInPictureControllerDidStopPictureInPicture(_ pictureInPictureController: AVPictureInPictureController) {
            logger.error("LALAL DID STOP")
            didShowHide(false)
        }

        //        func pictureInPictureController(_ pictureInPictureController: AVPictureInPictureController) async -> Bool {
        //            await MainActor.run {
        //                renderPip(false)
        //            }
        //            try? await Task.sleep(nanoseconds: 1000_000000)
        //            return true
        //        }
    }
}

struct CallViewRemote: UIViewRepresentable {
    var client: WebRTCClient
    var activeCall: Binding<WebRTCClient.Call?>

    init(client: WebRTCClient, activeCall: Binding<WebRTCClient.Call?>) {
        self.client = client
        self.activeCall = activeCall
    }

    func makeUIView(context: Context) -> UIView {
        let view = UIView()
        if let call = activeCall.wrappedValue {
            let remoteRenderer = RTCMTLVideoView(frame: view.frame)
            remoteRenderer.videoContentMode = .scaleAspectFill
            client.addRemoteRenderer(call, remoteRenderer)
            addSubviewAndResize(remoteRenderer, into: view)
        }
        return view
    }

//    func makeViewWithFrameRenderer(_ call: WebRTCClient.Call, _ view: UIView, _ context: Context) {
//        let pipRemoteRenderer = SampleBufferVideoCallView()
//        pipRemoteRenderer.contentMode = .scaleAspectFill
//        let pipVideoCallViewController = AVPictureInPictureVideoCallViewController()
//        pipVideoCallViewController.preferredContentSize = CGSize(width: 1080, height: 1920)
//        addSubviewAndResize(pipRemoteRenderer, into: pipVideoCallViewController.view)
//        
//        let pipContentSource = AVPictureInPictureController.ContentSource(
//            activeVideoCallSourceView: view,
//            contentViewController: pipVideoCallViewController
//        )
//        
//        let pipController = AVPictureInPictureController(contentSource: pipContentSource)
//        pipController.canStartPictureInPictureAutomaticallyFromInline = true
//        pipController.delegate = context.coordinator
//        
//        let frameRenderer = FrameRenderer()
//        context.coordinator.willShowHide = { show in
//            //show ? client.addRemoteRenderer(call, frameRenderer) : client.removeRemoteRenderer(call, frameRenderer)
//        }
//        client.addRemoteRenderer(call, frameRenderer)
//        DispatchQueue.main.async {
//            enablePip = { enable in
//                if enable != pipShown /* pipController.isPictureInPictureActive */ {
//                    if enable {
//                        pipController.startPictureInPicture()
//                        logger.debug("LALAL PIP TRY START")
//                    } else {
//                        pipController.stopPictureInPicture()
//                        client.removeRemoteRenderer(call, frameRenderer)
//                    }
//                    pipShown = enable
//                }
//            }
//        }
//
//        frameRenderer.renderFrame = { frame in
//            DispatchQueue.main.async {
//                logger.debug("LALAL RENDERED")
//                if #available(iOS 17.0, *) {
//                    pipRemoteRenderer.sampleBufferDisplayLayer.sampleBufferRenderer.enqueue(frame)
//                } else {
//                    pipRemoteRenderer.sampleBufferDisplayLayer.enqueue(frame)
//                }
//                enablePip(true)
//            }
//        }
//        logger.debug("LALAL PIP possible: \(pipController.isPictureInPicturePossible)")
//    }
//    
//    func makeCoordinator() -> Coordinator {
//        Coordinator()
//    }

    func updateUIView(_ view: UIView, context: Context) {
        logger.debug("CallView.updateUIView remote")
    }

    class SampleBufferVideoCallView: UIView {
        override class var layerClass: AnyClass {
            get { return AVSampleBufferDisplayLayer.self }
        }

        var sampleBufferDisplayLayer: AVSampleBufferDisplayLayer {
            return layer as! AVSampleBufferDisplayLayer
        }
    }
}

struct CallViewLocal: UIViewRepresentable {
    var client: WebRTCClient
    var activeCall: Binding<WebRTCClient.Call?>
    var localRendererAspectRatio: Binding<CGFloat?>

    init(client: WebRTCClient, activeCall: Binding<WebRTCClient.Call?>, localRendererAspectRatio: Binding<CGFloat?>) {
        self.client = client
        self.activeCall = activeCall
        self.localRendererAspectRatio = localRendererAspectRatio
    }

    func makeUIView(context: Context) -> UIView {
        let view = UIView()
        if let call = activeCall.wrappedValue {
            let localRenderer = RTCEAGLVideoView(frame: .zero)
            client.addLocalRenderer(call, localRenderer)
            client.startCaptureLocalVideo(call)
            addSubviewAndResize(localRenderer, into: view)
        }
        return view
    }

    func updateUIView(_ view: UIView, context: Context) {
        logger.debug("CallView.updateUIView local")
    }
}

private func addSubviewAndResize(_ view: UIView, into containerView: UIView) {
    containerView.addSubview(view)
    view.translatesAutoresizingMaskIntoConstraints = false
    containerView.addConstraints(NSLayoutConstraint.constraints(withVisualFormat: "H:|[view]|",
        options: [],
        metrics: nil,
        views: ["view": view]))

    containerView.addConstraints(NSLayoutConstraint.constraints(withVisualFormat: "V:|[view]|",
        options: [],
        metrics: nil,
        views: ["view": view]))
    containerView.layoutIfNeeded()
}
