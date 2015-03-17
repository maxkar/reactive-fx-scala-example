package ru.maxkar.reactive.wave


/**
 * Propagation node key. Keys are used to track dependencies between
 * pins during wave propagation. You could treat keys as
 * input node connectors.
 * @param pin pin associated with this key.
 */
final class Key private[wave](private[wave] val pin : Pin[_]) {
}
