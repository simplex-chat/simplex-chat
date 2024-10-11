//
// Created by Avently on 09.02.2023.
// Copyright (c) 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import WebRTC
import SimpleXChat
import AVKit

struct CallViewRemote: UIViewRepresentable {
    var client: WebRTCClient
    @ObservedObject var call: Call
    @State var enablePip: (Bool) -> Void = {_ in }
    @Binding var activeCallViewIsCollapsed: Bool
    @Binding var contentMode: UIView.ContentMode
    @Binding var pipShown: Bool

    func makeUIView(context: Context) -> UIView {
        let view = UIView()
        let remoteCameraRenderer = RTCMTLVideoView(frame: view.frame)
        remoteCameraRenderer.videoContentMode = contentMode
        remoteCameraRenderer.tag = 0

        let screenVideo = call.peerMediaSources.screenVideo
        let remoteScreenRenderer = RTCMTLVideoView(frame: view.frame)
        remoteScreenRenderer.videoContentMode = contentMode
        remoteScreenRenderer.tag = 1
        remoteScreenRenderer.alpha = screenVideo ? 1 : 0

        context.coordinator.cameraRenderer = remoteCameraRenderer
        context.coordinator.screenRenderer = remoteScreenRenderer
        client.addRemoteCameraRenderer(remoteCameraRenderer)
        client.addRemoteScreenRenderer(remoteScreenRenderer)
        if screenVideo {
            addSubviewAndResize(remoteScreenRenderer, remoteCameraRenderer, into: view)
        } else {
            addSubviewAndResize(remoteCameraRenderer, remoteScreenRenderer, into: view)
        }

        if AVPictureInPictureController.isPictureInPictureSupported() {
            makeViewWithRTCRenderer(remoteCameraRenderer, remoteScreenRenderer, view, context)
        }
        return view
    }
    
    func makeViewWithRTCRenderer(_ remoteCameraRenderer: RTCMTLVideoView, _ remoteScreenRenderer: RTCMTLVideoView, _ view: UIView, _ context: Context) {
        let pipRemoteCameraRenderer = RTCMTLVideoView(frame: view.frame)
        pipRemoteCameraRenderer.videoContentMode = .scaleAspectFill

        let pipRemoteScreenRenderer = RTCMTLVideoView(frame: view.frame)
        pipRemoteScreenRenderer.videoContentMode = .scaleAspectFill

        let pipVideoCallViewController = AVPictureInPictureVideoCallViewController()
        pipVideoCallViewController.preferredContentSize = CGSize(width: 1080, height: 1920)
        let pipContentSource = AVPictureInPictureController.ContentSource(
            activeVideoCallSourceView: view,
            contentViewController: pipVideoCallViewController
        )
        
        let pipController = AVPictureInPictureController(contentSource: pipContentSource)
        pipController.canStartPictureInPictureAutomaticallyFromInline = true
        pipController.delegate = context.coordinator
        context.coordinator.pipController = pipController
        context.coordinator.willShowHide = { show in
            if show {
                client.addRemoteCameraRenderer(pipRemoteCameraRenderer)
                client.addRemoteScreenRenderer(pipRemoteScreenRenderer)
                context.coordinator.relayout()
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
                    activeCallViewIsCollapsed = true
                }
            } else {
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.05) {
                    activeCallViewIsCollapsed = false
                }
            }
        }
        context.coordinator.didShowHide = { show in
            if show {
                remoteCameraRenderer.isHidden = true
                remoteScreenRenderer.isHidden = true
            } else {
                client.removeRemoteCameraRenderer(pipRemoteCameraRenderer)
                client.removeRemoteScreenRenderer(pipRemoteScreenRenderer)
                remoteCameraRenderer.isHidden = false
                remoteScreenRenderer.isHidden = false
            }
            pipShown = show
        }
        context.coordinator.relayout = {
            let camera = call.peerMediaSources.camera
            let screenVideo = call.peerMediaSources.screenVideo
            pipRemoteCameraRenderer.alpha = camera ? 1 : 0
            pipRemoteScreenRenderer.alpha = screenVideo ? 1 : 0
            if screenVideo {
                addSubviewAndResize(pipRemoteScreenRenderer, pipRemoteCameraRenderer, pip: true, into: pipVideoCallViewController.view)
            } else {
                addSubviewAndResize(pipRemoteCameraRenderer, pipRemoteScreenRenderer, pip: true, into: pipVideoCallViewController.view)
            }
            (pipVideoCallViewController.view.subviews[0] as! RTCMTLVideoView).videoContentMode = contentMode
            (pipVideoCallViewController.view.subviews[1] as! RTCMTLVideoView).videoContentMode = .scaleAspectFill
        }
        DispatchQueue.main.async {
            enablePip = { enable in
                if enable != pipShown /* pipController.isPictureInPictureActive */ {
                    if enable {
                        pipController.startPictureInPicture()
                    } else {
                        pipController.stopPictureInPicture()
                    }
                }
            }
        }
    }
    
    func makeCoordinator() -> Coordinator {
        Coordinator(client)
    }

    func updateUIView(_ view: UIView, context: Context) {
        logger.debug("CallView.updateUIView remote")
        let camera = view.subviews.first(where: { $0.tag == 0 })!
        let screen = view.subviews.first(where: { $0.tag == 1 })!
        let screenVideo = call.peerMediaSources.screenVideo
        if screenVideo && screen.alpha == 0 {
            screen.alpha = 1
            addSubviewAndResize(screen, camera, into: view)
        } else if !screenVideo && screen.alpha == 1 {
            screen.alpha = 0
            addSubviewAndResize(camera, screen, into: view)
        }
        (view.subviews[0] as! RTCMTLVideoView).videoContentMode = contentMode
        (view.subviews[1] as! RTCMTLVideoView).videoContentMode = .scaleAspectFill

        camera.alpha = call.peerMediaSources.camera ? 1 : 0
        screen.alpha = call.peerMediaSources.screenVideo ? 1 : 0

        DispatchQueue.main.async {
            if activeCallViewIsCollapsed != pipShown {
                enablePip(activeCallViewIsCollapsed)
            } else if pipShown {
                context.coordinator.relayout()
            }
        }
    }
    
    // MARK: - Coordinator
    class Coordinator: NSObject, AVPictureInPictureControllerDelegate {
        var cameraRenderer: RTCMTLVideoView?
        var screenRenderer: RTCMTLVideoView?
        var client: WebRTCClient
        var pipController: AVPictureInPictureController? = nil
        var willShowHide: (Bool) -> Void = { _ in }
        var didShowHide: (Bool) -> Void = { _ in }
        var relayout: () -> Void = {}

        required init(_ client: WebRTCClient) {
            self.client = client
        }

        func pictureInPictureControllerWillStartPictureInPicture(_ pictureInPictureController: AVPictureInPictureController) {
            willShowHide(true)
        }

        func pictureInPictureControllerDidStartPictureInPicture(_ pictureInPictureController: AVPictureInPictureController) {
            didShowHide(true)
        }

        func pictureInPictureController(_ pictureInPictureController: AVPictureInPictureController, failedToStartPictureInPictureWithError error: Error) {
            logger.error("PiP failed to start: \(error.localizedDescription)")
        }
        
        func pictureInPictureControllerWillStopPictureInPicture(_ pictureInPictureController: AVPictureInPictureController) {
            willShowHide(false)
        }
        
        func pictureInPictureControllerDidStopPictureInPicture(_ pictureInPictureController: AVPictureInPictureController) {
            didShowHide(false)
        }

        deinit {
            // TODO: deinit is not called when changing call type from audio to video and back,
            // which causes many renderers can be created and added to stream (if enabling/disabling
            // video while not yet connected in outgoing call)
            pipController?.stopPictureInPicture()
            pipController?.canStartPictureInPictureAutomaticallyFromInline = false
            pipController?.contentSource = nil
            pipController?.delegate = nil
            pipController = nil
            if let cameraRenderer {
                client.removeRemoteCameraRenderer(cameraRenderer)
            }
            if let screenRenderer {
                client.removeRemoteScreenRenderer(screenRenderer)
            }
        }
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
    var localRendererAspectRatio: Binding<CGFloat?>
    @State var pipStateChanged: (Bool) -> Void = {_ in }
    @Binding var pipShown: Bool

    init(client: WebRTCClient, localRendererAspectRatio: Binding<CGFloat?>, pipShown: Binding<Bool>) {
        self.client = client
        self.localRendererAspectRatio = localRendererAspectRatio
        self._pipShown = pipShown
    }

    func makeUIView(context: Context) -> UIView {
        let view = UIView()
        let localRenderer = RTCEAGLVideoView(frame: .zero)
        context.coordinator.renderer = localRenderer
        client.addLocalRenderer(localRenderer)
        addSubviewAndResize(localRenderer, nil, into: view)
        DispatchQueue.main.async {
            pipStateChanged = { shown in
                localRenderer.isHidden = shown
            }
        }
        return view
    }

    func makeCoordinator() -> Coordinator {
        Coordinator(client)
    }

    func updateUIView(_ view: UIView, context: Context) {
        logger.debug("CallView.updateUIView local")
        pipStateChanged(pipShown)
    }

    // MARK: - Coordinator
    class Coordinator: NSObject, AVPictureInPictureControllerDelegate {
        var renderer: RTCEAGLVideoView?
        var client: WebRTCClient

        required init(_ client: WebRTCClient) {
            self.client = client
        }

        deinit {
            if let renderer {
                client.removeLocalRenderer(renderer)
            }
        }
    }
}

private func addSubviewAndResize(_ fullscreen: UIView, _ end: UIView?, pip: Bool = false, into containerView: UIView) {
    if containerView.subviews.firstIndex(of: fullscreen) == 0 && ((end == nil && containerView.subviews.count == 1) || (end != nil && containerView.subviews.firstIndex(of: end!) == 1)) {
        // Nothing to do, elements on their places
        return
    }
    containerView.removeConstraints(containerView.constraints)
    containerView.subviews.forEach { sub in sub.removeFromSuperview()}

    containerView.addSubview(fullscreen)
    fullscreen.translatesAutoresizingMaskIntoConstraints = false
    fullscreen.layer.cornerRadius = 0
    fullscreen.layer.masksToBounds = false

    if let end {
        containerView.addSubview(end)
        end.translatesAutoresizingMaskIntoConstraints = false
        end.layer.cornerRadius = pip ? 8 : 10
        end.layer.masksToBounds = true
    }

    let constraintFullscreenV = NSLayoutConstraint.constraints(
        withVisualFormat: "V:|[fullscreen]|",
        options: [],
        metrics: nil,
        views: ["fullscreen": fullscreen]
    )
    let constraintFullscreenH = NSLayoutConstraint.constraints(
        withVisualFormat: "H:|[fullscreen]|",
        options: [],
        metrics: nil,
        views: ["fullscreen": fullscreen]
    )

    containerView.addConstraints(constraintFullscreenV)
    containerView.addConstraints(constraintFullscreenH)

    if let end {
        let constraintEndWidth = NSLayoutConstraint(
            item: end, attribute: .width, relatedBy: .equal, toItem: containerView, attribute: .width, multiplier: pip ? 0.5 : 0.3, constant: 0
        )
        let constraintEndHeight = NSLayoutConstraint(
            item: end, attribute: .height, relatedBy: .equal, toItem: containerView, attribute: .width, multiplier: pip ? 0.5 * 1.33 : 0.3 * 1.33, constant: 0
        )
        let constraintEndX = NSLayoutConstraint(
            item: end, attribute: .leading, relatedBy: .equal, toItem: containerView, attribute: .trailing, multiplier: pip ? 0.5 : 0.7, constant: pip ? -8 : -17
        )
        let constraintEndY = NSLayoutConstraint(
            item: end, attribute: .bottom, relatedBy: .equal, toItem: containerView, attribute: .bottom, multiplier: 1, constant: pip ? -8 : -92
        )
        containerView.addConstraint(constraintEndWidth)
        containerView.addConstraint(constraintEndHeight)
        containerView.addConstraint(constraintEndX)
        containerView.addConstraint(constraintEndY)
    }
    containerView.layoutIfNeeded()
}
