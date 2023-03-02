//
// Created by Avently on 09.02.2023.
// Copyright (c) 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import WebRTC
import SimpleXChat

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

    func updateUIView(_ view: UIView, context: Context) {
        logger.debug("CallView.updateUIView remote")
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
