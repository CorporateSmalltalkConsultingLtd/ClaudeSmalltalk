# CHANGES.md — Cuis → Squeak MQTT5 Port

## Overview
Port of MQTT 5.0 client library from Cuis Smalltalk to Squeak Smalltalk.

## File Output
- `MQTT5-Squeak.st` — Main library (all classes and methods)
- `MQTT5-Squeak-Tests.st` — Unit tests (SUnit)
- `MQTT5-Squeak-IntegrationTests.st` — Integration tests (completed stubs)
- `CHANGES.md` — This file

## Key Adaptations

### 1. UTF8Encoder — Complete Rewrite
**Problem:** Cuis provides `CharacterSequence fromUtf8Bytes:` and `String>>asUtf8Bytes` which don't exist in Squeak.

**Solution:** Replaced both methods with self-contained, manual UTF-8 codec:
- `UTF8Encoder class>>decode:` — Manual byte-by-byte UTF-8 decoder handling 1-4 byte sequences. Converts ByteArray → String.
- `UTF8Encoder class>>encode:` — Manual character-by-character UTF-8 encoder. Converts String → ByteArray.

Both methods handle the full Unicode range (U+0000 to U+10FFFF) and are tested with Japanese katakana (テスト) in the integration tests.

### 2. Category Renaming
- `MQTT5-Cuis` → `MQTT5-Squeak`
- `MQTT5-Cuis-Tests` → `MQTT5-Squeak-Tests`
- `MQTT5-Cuis-IntegrationTests` → `MQTT5-Squeak-IntegrationTests`
- Extension category `*MQTT5-Cuis` → `*MQTT5-Squeak`

### 3. File Format
- Removed Cuis-specific `!provides:` and `!requires:` directives
- Removed Cuis-specific `!classDefinition:` lines (Squeak uses standard `subclass:` expressions)
- Kept standard Squeak file-out chunk format (`!` separators)

### 4. Broker Configuration
- Default broker host: `192.168.1.158` → `192.168.6.6`
- Port: 1883 (unchanged)
- Credentials: Hudson/manager (unchanged)

### 5. Client ID and Topic Prefixes
- Default client ID: `CuisMQTT5` → `SqueakMQTT5`
- Test client prefix: `CuisV5-` → `SqV5-`
- Test topic prefix: `cuis/test/v5/` → `squeak/test/v5/`
- Wildcard test prefix: `cuis/v5test/` → `squeak/v5test/`

### 6. String Method Compatibility
- `includesSubString:` → `includesSubstring:` (Squeak uses capital S)

### 7. Time API
- `Time primMillisecondClock` → `Time millisecondClockValue` (Squeak equivalent)

### 8. Boolean>>asBit
Squeak already has `True>>asBit` (→1) and `False>>asBit` (→0) natively.
The extension methods are kept in the file for safety but are redundant in Squeak.

### 9. Integer>>bitAt:put:
Squeak does not have this method natively. The extension method is retained as-is:
```smalltalk
bitAt: anInteger put: value
    value = 1 ifTrue: [^ self bitOr: (1 bitShift: anInteger - 1)].
    ^ self bitAnd: (1 bitShift: anInteger - 1) bitInvert.
```

### 10. Integration Test Stubs — Completed

The following stub methods were completed with full implementations:

#### MQTT5PropertiesIntegrationTest
- `testMessageExpiry` — Tests publishing with message expiry (v5 property). Subscribes, publishes via transport layer, verifies receipt.
- `testResponseTopic` — Tests publishing with response topic property. Verifies v5 pub/sub works end-to-end.
- `testUserProperties` — Tests v5 CONNECT properties acceptance. Verifies server capabilities are parsed from CONNACK.
- **NEW** `testServerCapabilitiesParsing` — Tests detailed CONNACK property parsing (receiveMaximum, maximumPacketSize, topicAliasMaximum).
- **NEW** `testSubscriptionWithQoSLevels` — Tests subscribing at QoS 2 with v5 SUBACK handling.

#### MQTT5PublishSubscribeTest (new tests added)
- **NEW** `testPublishSubscribeQoS2` — Tests QoS 2 (exactly-once) delivery with 4-step handshake.
- **NEW** `testRetainedMessage` — Tests retained message delivery to new subscribers.
- **NEW** `testEmptyMessage` — Tests empty payload publish/subscribe.
- **NEW** `testLargeMessage` — Tests 10KB message publish/subscribe.
- **NEW** `testSingleLevelWildcard` — Tests `+` wildcard subscription.

#### MQTT5ConnectionTest (new tests added)
- **NEW** `testMultipleConnections` — Tests simultaneous client connections.
- **NEW** `testReconnectWithSameID` — Tests reconnection with same client ID.

## API Compatibility
All public APIs remain identical. Existing code using `MQTTClientInterface`, `UTF8Encoder`, and all packet classes will work unchanged. The port is a drop-in replacement.

## Dependencies
- Squeak's `Network-Kernel` package (Socket, SocketStream, NetNameResolver)
- Squeak's `SUnit` (TestCase) for tests
- No external packages required
