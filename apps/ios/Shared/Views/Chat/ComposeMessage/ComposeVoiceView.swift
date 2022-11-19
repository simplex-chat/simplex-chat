//
//  ComposeVoiceView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 18.11.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import AVFoundation

class RecordVC: UIViewController, AVAudioRecorderDelegate, AVAudioPlayerDelegate {
    @IBOutlet var recordingTimeLabel: UILabel!
    @IBOutlet var recordBtnRef: UIButton!
    @IBOutlet var playBtnRef: UIButton!

    var audioRecorder: AVAudioRecorder!
    var audioPlayer : AVAudioPlayer!
    var meterTimer:Timer!
    var isAudioRecordingGranted: Bool!
    var isRecording = false
    var isPlaying = false

    override func viewDidLoad() {
        super.viewDidLoad()
        checkRecordPermission()
        print("#################### in viewDidLoad")
    }

    func checkRecordPermission() {
        switch AVAudioSession.sharedInstance().recordPermission {
        case AVAudioSession.RecordPermission.granted:
            isAudioRecordingGranted = true
            break
        case AVAudioSession.RecordPermission.denied:
            isAudioRecordingGranted = false
            break
        case AVAudioSession.RecordPermission.undetermined:
            AVAudioSession.sharedInstance().requestRecordPermission({ (allowed) in
                if allowed {
                    self.isAudioRecordingGranted = true
                } else {
                    self.isAudioRecordingGranted = false
                }
            })
            break
        default:
            break
        }
    }

    @IBAction func startRecording(_ sender: UIButton) {
        if (isRecording) {
            finishAudioRecording(success: true)
            recordBtnRef.setTitle("Record", for: .normal)
            playBtnRef.isEnabled = true
            isRecording = false
        } else if isAudioRecordingGranted {
            audioRecorder = startAudioRecording(url: getAppFilePath(generateNewFileName("voice", "m4a")))
            meterTimer = Timer.scheduledTimer(timeInterval: 0.1, target: self, selector: #selector(self.updateAudioMeter(timer:)), userInfo: nil, repeats: true)
            recordBtnRef.setTitle("Stop", for: .normal)
            playBtnRef.isEnabled = false
            isRecording = true
        } else {
            logger.error("RecordVC setupRecorder error, no access to use microphone.")
            // display_alert(msg_title: "Error", msg_desc: "Don't have access to use your microphone.", action_title: "OK")
        }
    }

    @objc func updateAudioMeter(timer: Timer) {
        if audioRecorder.isRecording {
            let hr = Int((audioRecorder.currentTime / 60) / 60)
            let min = Int(audioRecorder.currentTime / 60)
            let sec = Int(audioRecorder.currentTime.truncatingRemainder(dividingBy: 60))
            let totalTimeString = String(format: "%02d:%02d:%02d", hr, min, sec)
            recordingTimeLabel.text = totalTimeString
            audioRecorder.updateMeters()
        }
    }

    func finishAudioRecording(success: Bool) {
        if success {
            audioRecorder.stop()
            audioRecorder = nil
            meterTimer.invalidate()
            print("recorded successfully.")
        } else {
            logger.error("RecordVC finishAudioRecording error, recording failed")
            // display_alert(msg_title: "Error", msg_desc: "Recording failed.", action_title: "OK")
        }
    }

    func preparePlay(_ fileName: String) {
        do {
            audioPlayer = try AVAudioPlayer(contentsOf: getAppFilePath(fileName))
            audioPlayer.delegate = self
            audioPlayer.prepareToPlay()
        } catch {
            logger.error("RecordVC preparePlay error")
        }
    }

    @IBAction func playPecording(_ fileName: String, _ sender: Any) {
        if isPlaying {
            audioPlayer.stop()
            recordBtnRef.isEnabled = true
            playBtnRef.setTitle("Play", for: .normal)
            isPlaying = false
        } else {
            if FileManager.default.fileExists(atPath: getAppFilePath(fileName).path) { // use getLoadedFilePath
                recordBtnRef.isEnabled = false
                playBtnRef.setTitle("pause", for: .normal)
                preparePlay(fileName)
                audioPlayer.play()
                isPlaying = true
            } else {
                logger.error("RecordVC finishAudioRecording error, recording failed")
                // display_alert(msg_title: "Error", msg_desc: "Audio file is missing.", action_title: "OK")
            }
        }
    }

    func audioRecorderDidFinishRecording(_ recorder: AVAudioRecorder, successfully flag: Bool) {
        if !flag {
            finishAudioRecording(success: false)
        }
        playBtnRef.isEnabled = true
    }

    func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool) {
        recordBtnRef.isEnabled = true
    }
}

struct RecordView: UIViewControllerRepresentable {
    typealias UIViewControllerType = RecordVC

    func makeUIViewController(context: Context) -> RecordVC {
        let vc = RecordVC()
        // Do some configurations here if needed.
        return vc
    }

    func updateUIViewController(_ uiViewController: RecordVC, context: Context) {
        // Updates the state of the specified view controller with new information from SwiftUI.
    }
}

struct ComposeVoiceView: View {
    @State var isPresented = false

    var body: some View {
        VStack {
            RecordView()
        }
    }
}

struct ComposeVoiceView_Previews: PreviewProvider {
    static var previews: some View {
        ComposeVoiceView()
    }
}
