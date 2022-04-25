let incomingVideo = document.getElementById("incoming-video-stream")
let outgoingVideo = document.getElementById("outgoing-video-stream")
incomingVideo.style.opacity = 0
incomingVideo.onplaying = () => { incomingVideo.style.opacity = 1 }


function startCall() {
    navigator.getUserMedia({
        audio: true,
        video: {
            frameRate: 24,
            width: {
                min: 480, ideal: 720, max: 1280
            },
            aspectRatio: 1.33
        }
    }, (stream) => {
        console.log("Getting video data from local device.")
        outgoingVideo.srcObject = stream
        // TODO get external stream and display it
        // incomingVideo.srcObject = incomingStream
    })
}

startCall()
