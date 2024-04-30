/*
 * Copyright 2021 IceRock MAG Inc. Use of this source code is governed by the Apache 2.0 license.
 */

package chat.simplex.common.other.mokoresources

import chat.simplex.res.MR.assets.resourcesClassLoader
import dev.icerock.moko.resources.ImageResource
import org.apache.batik.transcoder.*
import org.apache.batik.transcoder.image.PNGTranscoder
import java.awt.image.BufferedImage
import java.io.File
import java.io.FileNotFoundException
import java.io.InputStream
import javax.imageio.ImageIO

private var cache: Pair<String, BufferedImage>? = null

// Get rid of this file when we update to moko-resources >= 0.24.0 and use `image` instead of `customImage()`
// See https://github.com/icerockdev/moko-resources/commit/93900ca2690d2c70cf4db24902a4b89f30877176
fun ImageResource.customImage(): BufferedImage {
  if (cache?.first == filePath) return cache!!.second

  val stream = resourcesClassLoader.getResourceAsStream(filePath)
    ?: throw FileNotFoundException("Couldn't open resource as stream at: $filePath")
  val res = stream.use {
    if (filePath.endsWith(".svg", ignoreCase = true)) {
      readSvg(it)
    } else {
      ImageIO.read(it)
    }
  }
  cache = filePath to res
  return res
}

private fun readSvg(
    inputStream: InputStream
  ): BufferedImage {
  // Create a PNG transcoder.
  val t: Transcoder = PNGTranscoder()
  // Create the transcoder input.
  val input = TranscoderInput(inputStream)

  // Create the transcoder output.
  val tempFile: File = File.createTempFile("moko-resources", ".png")

  try {
    tempFile.outputStream().use {
      val output = TranscoderOutput(it)
      t.transcode(input, output)
    }
    return tempFile.inputStream().use {
      ImageIO.read(it)
    }
  } finally {
    tempFile.delete()
  }
}
