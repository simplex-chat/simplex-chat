// https://stackoverflow.com/a/76840267

import Foundation
import WebRTC
import AVKit
import VideoToolbox
import Accelerate

// Define closure type for handling CMSampleBuffer, orientation, scaleFactor
typealias CMSampleBufferRenderer = (CMSampleBuffer, CGImagePropertyOrientation, CGFloat) -> ()

// Define closure variables for handling CMSampleBuffer from FrameRenderer
var getCMSampleBufferFromFrameRenderer: CMSampleBufferRenderer = { _,_,_ in }
var getCMSampleBufferFromFrameRendererForPIP: CMSampleBufferRenderer = { _,_,_ in }
var getLocalVideoCMSampleBufferFromFrameRenderer:
CMSampleBufferRenderer = { _,_,_ in }

// Define the FrameRenderer class responsible for rendering video frames
class FrameRenderer: NSObject, RTCVideoRenderer {
    // VARIABLES
    var scaleFactor: CGFloat?
    var frameImage = UIImage()
    var videoFormatDescription: CMFormatDescription?
    var renderFrame: ((CMSampleBuffer) -> ())?
    private var ciContext = CIContext()
        
    // Set the aspect ratio based on the size
    func setSize(_ size: CGSize) {
        self.scaleFactor = size.height > size.width ? size.height / size.width : size.width / size.height
    }
    
    // Render a video frame received from WebRTC
    func renderFrame(_ frame: RTCVideoFrame?) {
        guard let pixelBuffer = self.createPixelBufferFrom(image: CIImage(cgImage: UIImage(named: "icon-transparent")!.cgImage!)) /*self.getCVPixelBuffer(frame: frame)*/ else {
            logger.debug("LALAL PIXELBUFFER NIL")
            return
        }
        
        // Extract timing information from the frame and create a CMSampleBuffer
        let timingInfo = covertFrameTimestampToTimingInfo(frame: frame)!
        let cmSampleBuffer = self.createSampleBufferFrom(pixelBuffer: pixelBuffer, timingInfo: timingInfo)!
        
        // Determine the video orientation and handle the CMSampleBuffer accordingly
        let oriented: CGImagePropertyOrientation?
        switch frame!.rotation.rawValue {
        case RTCVideoRotation._0.rawValue:
            oriented = .right
        case RTCVideoRotation._90.rawValue:
            oriented = .right
        case RTCVideoRotation._180.rawValue:
            oriented = .right
        case RTCVideoRotation._270.rawValue:
            oriented = .left
        default:
            oriented = .right
        }
        
        // Pass the CMSampleBuffer to the appropriate closure based on the user ID
        //if objNewUserDM?.userId == self.recUserID {
            //getLocalVideoCMSampleBufferFromFrameRenderer(cmSampleBuffer, oriented!, self.scaleFactor!, self.recUserID)
        //} else {
            //getCMSampleBufferFromFrameRenderer(cmSampleBuffer, oriented!, self.scaleFactor!, self.recUserID)
            //getCMSampleBufferFromFrameRendererForPIP(cmSampleBuffer, oriented!, self.scaleFactor!, self.recUserID)
        //}
        
        // Call the didGetFrame closure if it exists
        if let closure = renderFrame {
            closure(cmSampleBuffer)
        }
    }
    
    // Function to create a CVPixelBuffer from a CIImage
    func createPixelBufferFrom(image: CIImage) -> CVPixelBuffer? {
        let attrs = [
            kCVPixelBufferCGImageCompatibilityKey: false,
            kCVPixelBufferCGBitmapContextCompatibilityKey: false,
            kCVPixelBufferWidthKey: 100,
            kCVPixelBufferHeightKey: 200
        ] as CFDictionary
        
        var pixelBuffer: CVPixelBuffer?
        let status = CVPixelBufferCreate(kCFAllocatorDefault, 100, 200, kCVPixelFormatType_32BGRA, attrs, &pixelBuffer)
        
        if status == kCVReturnSuccess {
            self.ciContext.render(image, to: pixelBuffer!)
            return pixelBuffer
        } else {
            // Failed to create a CVPixelBuffer
            logger.error("Error creating CVPixelBuffer.")
            return nil
        }
    }
    
    // Function to create a CVPixelBuffer from a CIImage using an existing CVPixelBuffer
    func buffer(from image: CIImage, oldCVPixelBuffer: CVPixelBuffer) -> CVPixelBuffer? {
        let attrs = [
            kCVPixelBufferMetalCompatibilityKey: kCFBooleanTrue,
            kCVPixelBufferCGImageCompatibilityKey: kCFBooleanTrue,
            kCVPixelBufferCGBitmapContextCompatibilityKey: kCFBooleanTrue
        ] as CFDictionary
        
        var pixelBuffer: CVPixelBuffer?
        let status = CVPixelBufferCreate(kCFAllocatorDefault, Int(image.extent.width), Int(image.extent.height), kCVPixelFormatType_32BGRA, attrs, &pixelBuffer)
        
        if status == kCVReturnSuccess {
            oldCVPixelBuffer.propagateAttachments(to: pixelBuffer!)
            return pixelBuffer
        } else {
            // Failed to create a CVPixelBuffer
            logger.error("Error creating CVPixelBuffer.")
            return nil
        }
    }
    
    /// Convert RTCVideoFrame to CVPixelBuffer
    func getCVPixelBuffer(frame: RTCVideoFrame?) -> CVPixelBuffer? {
        if let cvPixelBuffer = (frame?.buffer as? RTCI420Buffer)?.convertToCVPixelBuffer() {
            return cvPixelBuffer
        }
        
        return (frame?.buffer as? RTCCVPixelBuffer)?.pixelBuffer
    }
    /// Convert RTCVideoFrame to CMSampleTimingInfo
    func covertFrameTimestampToTimingInfo(frame: RTCVideoFrame?) -> CMSampleTimingInfo? {
        let scale = CMTimeScale(NSEC_PER_SEC)
        let pts = CMTime(value: CMTimeValue(Double(frame!.timeStamp) * Double(scale)), timescale: scale)
        let timingInfo = CMSampleTimingInfo(duration: CMTime.invalid,
                                            presentationTimeStamp: pts,
                                            decodeTimeStamp: CMTime.invalid)
        return timingInfo
    }
    
    /// Convert CVPixelBuffer to CMSampleBuffer
    func createSampleBufferFrom(pixelBuffer: CVPixelBuffer, timingInfo: CMSampleTimingInfo) -> CMSampleBuffer? {
        var sampleBuffer: CMSampleBuffer?
        let now = CMTime (seconds: CACurrentMediaTime(), preferredTimescale: 120)
        //var info = CMSampleTimingInfo (duration: .init(seconds: 1, preferredTimescale: 60), presentationTimeStamp: now, decodeTimeStamp: now);
//        var pixelBuffer: CVPixelBuffer? = nil
//        CVPixelBufferCreate(kCFAllocatorDefault, 1280, 720, kCVPixelFormatType_32BGRA, nil, &pixelBuffer);

//        var formatDesc: CMFormatDescription? = nil;
//        CMVideoFormatDescriptionCreateForImageBuffer(allocator: kCFAllocatorDefault, imageBuffer: pixelBuffer!, formatDescriptionOut: &formatDesc);
//        CMSampleBufferCreateReadyWithImageBuffer(allocator: kCFAllocatorDefault,
//                                                 imageBuffer: pixelBuffer!,
//                                                 formatDescription: formatDesc!,
//                                                 sampleTiming: &info,
//                                                 sampleBufferOut: &sampleBuffer);
//        return sampleBuffer
        
        
//        
        var info = timingInfo
        var formatDescription: CMFormatDescription? = nil
        CMVideoFormatDescriptionCreateForImageBuffer(allocator: kCFAllocatorDefault, imageBuffer: pixelBuffer, formatDescriptionOut: &formatDescription)
        
        let osStatus = CMSampleBufferCreateReadyWithImageBuffer(
                                                                allocator: kCFAllocatorDefault,
                                                                imageBuffer: pixelBuffer,
                                                                formatDescription: formatDescription!,
                                                                sampleTiming: &info,
                                                                sampleBufferOut: &sampleBuffer
        )
        
        // Print out errors
//        if osStatus == kCMSampleBufferError_AllocationFailed {
//            logger.debug("osStatus == kCMSampleBufferError_AllocationFailed")
//        }
//        if osStatus == kCMSampleBufferError_RequiredParameterMissing {
//            logger.debug("osStatus == kCMSampleBufferError_RequiredParameterMissing")
//        }
//        if osStatus == kCMSampleBufferError_AlreadyHasDataBuffer {
//            logger.debug("osStatus == kCMSampleBufferError_AlreadyHasDataBuffer")
//        }
//        if osStatus == kCMSampleBufferError_BufferNotReady {
//            logger.debug("osStatus == kCMSampleBufferError_BufferNotReady")
//        }
//        if osStatus == kCMSampleBufferError_SampleIndexOutOfRange {
//            logger.debug("osStatus == kCMSampleBufferError_SampleIndexOutOfRange")
//        }
//        if osStatus == kCMSampleBufferError_BufferHasNoSampleSizes {
//            logger.debug("osStatus == kCMSampleBufferError_BufferHasNoSampleSizes")
//        }
//        if osStatus == kCMSampleBufferError_BufferHasNoSampleTimingInfo {
//            logger.debug("osStatus == kCMSampleBufferError_BufferHasNoSampleTimingInfo")
//        }
//        if osStatus == kCMSampleBufferError_ArrayTooSmall {
//            logger.debug("osStatus == kCMSampleBufferError_ArrayTooSmall")
//        }
//        if osStatus == kCMSampleBufferError_InvalidEntryCount {
//            logger.debug("osStatus == kCMSampleBufferError_InvalidEntryCount")
//        }
//        if osStatus == kCMSampleBufferError_CannotSubdivide {
//            logger.debug("osStatus == kCMSampleBufferError_CannotSubdivide")
//        }
//        if osStatus == kCMSampleBufferError_SampleTimingInfoInvalid {
//            logger.debug("osStatus == kCMSampleBufferError_SampleTimingInfoInvalid")
//        }
//        if osStatus == kCMSampleBufferError_InvalidMediaTypeForOperation {
//            logger.debug("osStatus == kCMSampleBufferError_InvalidMediaTypeForOperation")
//        }
//        if osStatus == kCMSampleBufferError_InvalidSampleData {
//            logger.debug("osStatus == kCMSampleBufferError_InvalidSampleData")
//        }
//        if osStatus == kCMSampleBufferError_InvalidMediaFormat {
//            logger.debug("osStatus == kCMSampleBufferError_InvalidMediaFormat")
//        }
//        if osStatus == kCMSampleBufferError_Invalidated {
//            logger.debug("osStatus == kCMSampleBufferError_Invalidated")
//        }
//        if osStatus == kCMSampleBufferError_DataFailed {
//            logger.debug("osStatus == kCMSampleBufferError_DataFailed")
//        }
//        if osStatus == kCMSampleBufferError_DataCanceled {
//            logger.debug("osStatus == kCMSampleBufferError_DataCanceled")
//        }
        
        guard let buffer = sampleBuffer else {
            logger.error("SampleBuffer is nil")
            return nil
        }
        
        let attachments: NSArray = CMSampleBufferGetSampleAttachmentsArray(buffer, createIfNecessary: true)! as NSArray
        let dict: NSMutableDictionary = attachments[0] as! NSMutableDictionary
        dict[kCMSampleAttachmentKey_DisplayImmediately as NSString] = true as NSNumber
        
        return buffer
    }
}

extension RTCI420Buffer {
    func convertToCVPixelBuffer() -> CVPixelBuffer? {
        var newPixelBuffer: CVPixelBuffer!
        let res = CVPixelBufferCreate(
            kCFAllocatorDefault,
            Int(self.width),
            Int(self.height),
            kCVPixelFormatType_420YpCbCr8PlanarFullRange,
            nil,
            &newPixelBuffer)
        guard res == kCVReturnSuccess else { fatalError() }
        
        CVPixelBufferLockBaseAddress(newPixelBuffer, CVPixelBufferLockFlags(rawValue: 0))
        defer {
            CVPixelBufferUnlockBaseAddress(newPixelBuffer, CVPixelBufferLockFlags(rawValue: 0))
        }
        let yPlane = CVPixelBufferGetBaseAddressOfPlane(newPixelBuffer, 0)
        let uPlane = CVPixelBufferGetBaseAddressOfPlane(newPixelBuffer, 1)
        guard let uPlane = uPlane, let yPlane = yPlane else { return nil }
        //        debugPrint("LALAL planes \(yPlane)  \(uPlane)   \(uPlane - yPlane)   \(Int(self.width) * Int(self.height) / 2)")
        let w = CVPixelBufferGetWidthOfPlane(newPixelBuffer, 0)
        let h = CVPixelBufferGetHeightOfPlane(newPixelBuffer, 0)
        memcpy(yPlane, self.dataY, w * h)
        memcpy(uPlane, self.dataU, (w * h) / 2)
        //        memcpy(vPlane, self.dataV, w * h)
        return newPixelBuffer
    }
}

