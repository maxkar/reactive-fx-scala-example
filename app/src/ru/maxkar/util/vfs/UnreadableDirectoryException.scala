package ru.maxkar.util.vfs


/**
 * Exception notifying about failure to read directory.
 */
final class UnreadableDirectoryException(
      val path : java.io.File)
    extends java.io.IOException
