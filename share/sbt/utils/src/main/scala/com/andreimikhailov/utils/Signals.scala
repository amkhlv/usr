package com.andreimikhailov.utils

import java.nio.file.{Path => JNPath}

/**
  * Created by andrei on 25/12/16.
  */
case class Start()
case class Continue()
case class Stop()
case class FileEvent(filename: String)
