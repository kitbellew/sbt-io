/*
 * sbt IO
 * Copyright Scala Center, Lightbend, and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * SPDX-License-Identifier: Apache-2.0
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package sbt.internal.io
import java.io.IOException

import scala.util.control.NonFatal

private[sbt] object Retry {
  private lazy val limit = {
    val defaultLimit = 10
    try System.getProperty("sbt.io.retry.limit", defaultLimit.toString).toInt
    catch { case NonFatal(_) => defaultLimit }
  }
  private final val defaultSleepInMillis = 100

  /**
   * Retry on all non-fatal exceptions that are NOT listed in
   * the excludedExceptions list.
   */
  private[sbt] def apply[@specialized T](f: => T, excludedExceptions: Class[? <: Throwable]*): T =
    apply(f, limit, excludedExceptions*)

  /**
   * Retry on all non-fatal exceptions that are NOT listed in
   * the excludedExceptions list.
   */
  private[sbt] def apply[@specialized T](
      f: => T,
      limit: Int,
      excludedExceptions: Class[? <: Throwable]*,
  ): T = apply(f, limit, defaultSleepInMillis, excludedExceptions*)

  /**
   * Retry on all non-fatal exceptions that are NOT listed in
   * the excludedExceptions list.
   */
  private[sbt] def apply[@specialized T](
      f: => T,
      limit: Int,
      sleepInMillis: Long,
      excludedExceptions: Class[? <: Throwable]*,
  ): T = {
    def allowRetry(e: Throwable): Boolean = excludedExceptions match {
      case s if s.nonEmpty =>
        !excludedExceptions.exists(_.isAssignableFrom(e.getClass))
      case _ =>
        true
    }
    impl(limit = limit, sleepInMillis = sleepInMillis)(allowRetry)(f)
  }

  /**
   * Retry on all IOExceptions that are NOT listed in
   * the excludedExceptions list.
   * Non-IOException will immediately throw.
   */
  private[sbt] def io[@specialized A1](f: => A1, excludedExceptions: Class[? <: IOException]*): A1 =
    io(f, limit, excludedExceptions*)

  /**
   * Retry on all IOExceptions that are NOT listed in
   * the excludedExceptions list.
   * Non-IOException will immediately throw.
   */
  private[sbt] def io[@specialized A1](
      f: => A1,
      limit: Int,
      excludedExceptions: Class[? <: IOException]*,
  ): A1 = io(f, limit, defaultSleepInMillis, excludedExceptions*)

  /**
   * Retry on all IOExceptions that are NOT listed in
   * the excludedExceptions list.
   * Non-IOException will immediately throw.
   */
  private[sbt] def io[@specialized A1](
      f: => A1,
      limit: Int,
      sleepInMillis: Long,
      excludedExceptions: Class[? <: IOException]*,
  ): A1 = {
    def allowRetry(e: Throwable): Boolean =
      e match {
        case ioe: IOException =>
          excludedExceptions match {
            case s if s.nonEmpty =>
              !excludedExceptions.exists(_.isAssignableFrom(ioe.getClass))
            case _ =>
              true
          }
        case _ => false
      }
    impl(limit = limit, sleepInMillis = sleepInMillis)(allowRetry)(f)
  }

  private def impl[@specialized A1](
      limit: Int,
      sleepInMillis: Long,
  )(allowRetry: Throwable => Boolean)(f: => A1): A1 = {
    require(limit >= 1, "limit must be 1 or higher: was: " + limit)
    var attempt = 1
    var firstException: Throwable = null
    while (attempt <= limit) {
      try {
        return f
      } catch {
        case NonFatal(e) if allowRetry(e) =>
          if (firstException == null) firstException = e
          // https://github.com/sbt/io/issues/295
          // On Windows, we're seeing java.nio.file.AccessDeniedException with sleep(0).
          Thread.sleep(sleepInMillis)
          attempt += 1
      }
    }
    throw firstException
  }
}
