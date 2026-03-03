WriteStream subclass: #MQTTWriteStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

!MQTTWriteStream methodsFor: 'initialization'!
initialize
	super initialize.! !

!MQTTWriteStream class methodsFor: 'instance creation'!
on: aCollection
	^ super on: aCollection.! !


Error subclass: #MQTT5ProtocolException
	instanceVariableNames: 'reasonCode reasonString'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

!MQTT5ProtocolException methodsFor: 'accessing'!
messageText
	^ reasonString ifNil: [ 'MQTT Protocol Error: ' , (MQTT5ReasonCode descriptionFor: reasonCode) ].! !

!MQTT5ProtocolException methodsFor: 'accessing'!
reasonCode
	^ reasonCode.! !

!MQTT5ProtocolException methodsFor: 'accessing'!
reasonCode: anInteger
	reasonCode := anInteger.! !

!MQTT5ProtocolException methodsFor: 'accessing'!
reasonString
	^ reasonString.! !

!MQTT5ProtocolException methodsFor: 'accessing'!
reasonString: aString
	reasonString := aString.! !

!MQTT5ProtocolException methodsFor: 'printing'!
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ' reasonCode: '.
	reasonCode printOn: aStream.
	aStream nextPutAll: ' ('.
	aStream nextPutAll: (MQTT5Constants descriptionForReasonCode: reasonCode).
	aStream nextPutAll: ')'.
	reasonString ifNotNil: [
		aStream nextPutAll: ' reasonString: '.
		reasonString printOn: aStream ].! !

!MQTT5ProtocolException methodsFor: 'initialization'!
initialize
	super initialize.
	reasonCode := 0.! !

!MQTT5ProtocolException class methodsFor: 'instance creation'!
reasonCode: anInteger
	^ self new
		reasonCode: anInteger;
		yourself.! !

!MQTT5ProtocolException class methodsFor: 'instance creation'!
reasonCode: anInteger reasonString: aString
	^ self new
		reasonCode: anInteger;
		reasonString: aString;
		yourself.! !


Error subclass: #MQTTBadHeaderException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!


Error subclass: #MQTTBadPacketTypeException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!


Error subclass: #MQTTBadTopicException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!


Error subclass: #MQTTBrokerLookupFailure
	instanceVariableNames: 'brokerName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

!MQTTBrokerLookupFailure methodsFor: 'accessing'!
brokerName
	^ brokerName.! !

!MQTTBrokerLookupFailure methodsFor: 'accessing'!
brokerName: aString
	brokerName := aString.! !

!MQTTBrokerLookupFailure methodsFor: 'accessing'!
messageText
	^ 'Failed to resolve broker: ' , (brokerName ifNil: [ '(nil)' ]).! !

!MQTTBrokerLookupFailure class methodsFor: 'instance creation'!
brokerName: aString
	^ self new brokerName: aString.! !


Error subclass: #MQTTCONNECTException
	instanceVariableNames: 'connectReturnCode reasonCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

!MQTTCONNECTException methodsFor: 'accessing'!
connectReturnCode
	^ connectReturnCode.! !

!MQTTCONNECTException methodsFor: 'accessing'!
connectReturnCode: anInteger
	connectReturnCode := anInteger.! !

!MQTTCONNECTException methodsFor: 'accessing'!
messageText
	reasonCode > 0 ifTrue: [ ^ 'MQTT Connection Failed: ' , (MQTT5ReasonCode descriptionFor: reasonCode) ].
	connectReturnCode = 1 ifTrue: [ ^ 'Connection Refused: unacceptable protocol version' ].
	connectReturnCode = 2 ifTrue: [ ^ 'Connection Refused: identifier rejected' ].
	connectReturnCode = 3 ifTrue: [ ^ 'Connection Refused: server unavailable' ].
	connectReturnCode = 4 ifTrue: [ ^ 'Connection Refused: bad user name or password' ].
	connectReturnCode = 5 ifTrue: [ ^ 'Connection Refused: not authorized' ].
	^ 'Connection Failed: unknown error code ' , connectReturnCode printString.! !

!MQTTCONNECTException methodsFor: 'accessing'!
reasonCode
	^ reasonCode.! !

!MQTTCONNECTException methodsFor: 'accessing'!
reasonCode: anInteger
	reasonCode := anInteger.! !

!MQTTCONNECTException methodsFor: 'printing'!
printOn: aStream
	super printOn: aStream.
	connectReturnCode ifNotNil: [
		aStream nextPutAll: ' returnCode: '.
		connectReturnCode printOn: aStream ].
	reasonCode ifNotNil: [
		aStream nextPutAll: ' reasonCode: '.
		reasonCode printOn: aStream.
		aStream nextPutAll: ' ('.
		aStream nextPutAll: (MQTT5Constants descriptionForReasonCode: reasonCode).
		aStream nextPutAll: ')' ].! !

!MQTTCONNECTException methodsFor: 'initialization'!
initialize
	super initialize.
	connectReturnCode := 0.
	reasonCode := 0.! !

!MQTTCONNECTException class methodsFor: 'instance creation'!
connectReturnCode: anInteger
	^ (self new connectReturnCode: anInteger) signal.! !

!MQTTCONNECTException class methodsFor: 'instance creation'!
reasonCode: anInteger
	^ (self new reasonCode: anInteger) signal.! !


Error subclass: #MQTTConnectionException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!


Error subclass: #MQTTEmptyStreamErrorException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!


Object subclass: #MQTT5Constants
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

!MQTT5Constants class methodsFor: 'property identifiers'!
assignedClientIdentifier
	^ 18.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
authenticationData
	^ 22.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
authenticationMethod
	^ 21.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
contentType
	^ 3.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
correlationData
	^ 9.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
maximumPacketSize
	^ 39.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
maximumQoS
	^ 36.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
messageExpiryInterval
	^ 2.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
payloadFormatIndicator
	^ 1.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
reasonString
	^ 31.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
receiveMaximum
	^ 33.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
requestProblemInformation
	^ 23.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
requestResponseInformation
	^ 25.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
responseInformation
	^ 26.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
responseTopic
	^ 8.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
retainAvailable
	^ 37.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
serverKeepAlive
	^ 19.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
serverReference
	^ 28.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
sessionExpiryInterval
	^ 17.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
sharedSubscriptionAvailable
	^ 42.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
subscriptionIdentifier
	^ 11.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
subscriptionIdentifiersAvailable
	^ 41.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
topicAlias
	^ 35.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
topicAliasMaximum
	^ 34.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
userProperty
	^ 38.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
wildcardSubscriptionAvailable
	^ 40.! !

!MQTT5Constants class methodsFor: 'property identifiers'!
willDelayInterval
	^ 24.! !

!MQTT5Constants class methodsFor: 'property type info'!
propertyTypeFor: anInteger
	anInteger = 1 ifTrue: [ ^ #byte ].
	anInteger = 2 ifTrue: [ ^ #fourByteInteger ].
	anInteger = 3 ifTrue: [ ^ #utf8String ].
	anInteger = 8 ifTrue: [ ^ #utf8String ].
	anInteger = 9 ifTrue: [ ^ #binaryData ].
	anInteger = 11 ifTrue: [ ^ #variableByteInteger ].
	anInteger = 17 ifTrue: [ ^ #fourByteInteger ].
	anInteger = 18 ifTrue: [ ^ #utf8String ].
	anInteger = 19 ifTrue: [ ^ #twoByteInteger ].
	anInteger = 21 ifTrue: [ ^ #utf8String ].
	anInteger = 22 ifTrue: [ ^ #binaryData ].
	anInteger = 23 ifTrue: [ ^ #byte ].
	anInteger = 24 ifTrue: [ ^ #fourByteInteger ].
	anInteger = 25 ifTrue: [ ^ #byte ].
	anInteger = 26 ifTrue: [ ^ #utf8String ].
	anInteger = 28 ifTrue: [ ^ #utf8String ].
	anInteger = 31 ifTrue: [ ^ #utf8String ].
	anInteger = 33 ifTrue: [ ^ #twoByteInteger ].
	anInteger = 34 ifTrue: [ ^ #twoByteInteger ].
	anInteger = 35 ifTrue: [ ^ #twoByteInteger ].
	anInteger = 36 ifTrue: [ ^ #byte ].
	anInteger = 37 ifTrue: [ ^ #byte ].
	anInteger = 38 ifTrue: [ ^ #utf8StringPair ].
	anInteger = 39 ifTrue: [ ^ #fourByteInteger ].
	anInteger = 40 ifTrue: [ ^ #byte ].
	anInteger = 41 ifTrue: [ ^ #byte ].
	anInteger = 42 ifTrue: [ ^ #byte ].
	^ nil.! !

!MQTT5Constants class methodsFor: 'reason codes'!
descriptionForReasonCode: anInteger
	anInteger = 0 ifTrue: [ ^ 'Success' ].
	anInteger = 1 ifTrue: [ ^ 'Granted QoS 1' ].
	anInteger = 2 ifTrue: [ ^ 'Granted QoS 2' ].
	anInteger = 4 ifTrue: [ ^ 'Disconnect with Will Message' ].
	anInteger = 16 ifTrue: [ ^ 'No matching subscribers' ].
	anInteger = 17 ifTrue: [ ^ 'No subscription existed' ].
	anInteger = 24 ifTrue: [ ^ 'Continue authentication' ].
	anInteger = 25 ifTrue: [ ^ 'Re-authenticate' ].
	anInteger = 128 ifTrue: [ ^ 'Unspecified error' ].
	anInteger = 129 ifTrue: [ ^ 'Malformed Packet' ].
	anInteger = 130 ifTrue: [ ^ 'Protocol Error' ].
	anInteger = 131 ifTrue: [ ^ 'Implementation specific error' ].
	anInteger = 132 ifTrue: [ ^ 'Unsupported Protocol Version' ].
	anInteger = 133 ifTrue: [ ^ 'Client Identifier not valid' ].
	anInteger = 134 ifTrue: [ ^ 'Bad User Name or Password' ].
	anInteger = 135 ifTrue: [ ^ 'Not authorized' ].
	anInteger = 136 ifTrue: [ ^ 'Server unavailable' ].
	anInteger = 137 ifTrue: [ ^ 'Server busy' ].
	anInteger = 138 ifTrue: [ ^ 'Banned' ].
	anInteger = 139 ifTrue: [ ^ 'Server shutting down' ].
	anInteger = 140 ifTrue: [ ^ 'Bad authentication method' ].
	anInteger = 141 ifTrue: [ ^ 'Keep Alive timeout' ].
	anInteger = 142 ifTrue: [ ^ 'Session taken over' ].
	anInteger = 143 ifTrue: [ ^ 'Topic Filter invalid' ].
	anInteger = 144 ifTrue: [ ^ 'Topic Name invalid' ].
	anInteger = 145 ifTrue: [ ^ 'Packet Identifier in use' ].
	anInteger = 146 ifTrue: [ ^ 'Packet Identifier not found' ].
	anInteger = 147 ifTrue: [ ^ 'Receive Maximum exceeded' ].
	anInteger = 148 ifTrue: [ ^ 'Topic Alias invalid' ].
	anInteger = 149 ifTrue: [ ^ 'Packet too large' ].
	anInteger = 150 ifTrue: [ ^ 'Message rate too high' ].
	anInteger = 151 ifTrue: [ ^ 'Quota exceeded' ].
	anInteger = 152 ifTrue: [ ^ 'Administrative action' ].
	anInteger = 153 ifTrue: [ ^ 'Payload format invalid' ].
	anInteger = 154 ifTrue: [ ^ 'Retain not supported' ].
	anInteger = 155 ifTrue: [ ^ 'QoS not supported' ].
	anInteger = 156 ifTrue: [ ^ 'Use another server' ].
	anInteger = 157 ifTrue: [ ^ 'Server moved' ].
	anInteger = 158 ifTrue: [ ^ 'Shared Subscriptions not supported' ].
	anInteger = 159 ifTrue: [ ^ 'Connection rate exceeded' ].
	anInteger = 160 ifTrue: [ ^ 'Maximum connect time' ].
	anInteger = 161 ifTrue: [ ^ 'Subscription Identifiers not supported' ].
	anInteger = 162 ifTrue: [ ^ 'Wildcard Subscriptions not supported' ].
	^ 'Unknown reason code: ' , anInteger printString.! !

!MQTT5Constants class methodsFor: 'reason codes'!
isErrorReasonCode: anInteger
	^ anInteger >= 128.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeAdministrativeAction
	^ 152.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeBadAuthenticationMethod
	^ 140.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeBadUserNameOrPassword
	^ 134.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeBanned
	^ 138.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeClientIdentifierNotValid
	^ 133.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeConnectionRateExceeded
	^ 159.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeContinueAuthentication
	^ 24.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeDisconnectWithWillMessage
	^ 4.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeGrantedQoS0
	^ 0.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeGrantedQoS1
	^ 1.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeGrantedQoS2
	^ 2.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeImplementationSpecificError
	^ 131.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeKeepAliveTimeout
	^ 141.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeMalformedPacket
	^ 129.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeMaximumConnectTime
	^ 160.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeMessageRateTooHigh
	^ 150.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeNoMatchingSubscribers
	^ 16.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeNoSubscriptionExisted
	^ 17.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeNormalDisconnection
	^ 0.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeNotAuthorized
	^ 135.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodePacketIdentifierInUse
	^ 145.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodePacketIdentifierNotFound
	^ 146.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodePacketTooLarge
	^ 149.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodePayloadFormatInvalid
	^ 153.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeProtocolError
	^ 130.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeQoSNotSupported
	^ 155.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeQuotaExceeded
	^ 151.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeReAuthenticate
	^ 25.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeReceiveMaximumExceeded
	^ 147.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeRetainNotSupported
	^ 154.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeServerBusy
	^ 137.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeServerMoved
	^ 157.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeServerShuttingDown
	^ 139.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeServerUnavailable
	^ 136.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeSessionTakenOver
	^ 142.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeSharedSubscriptionsNotSupported
	^ 158.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeSubscriptionIdentifiersNotSupported
	^ 161.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeSuccess
	^ 0.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeTopicAliasInvalid
	^ 148.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeTopicFilterInvalid
	^ 143.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeTopicNameInvalid
	^ 144.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeUnspecifiedError
	^ 128.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeUnsupportedProtocolVersion
	^ 132.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeUseAnotherServer
	^ 156.! !

!MQTT5Constants class methodsFor: 'reason codes'!
reasonCodeWildcardSubscriptionsNotSupported
	^ 162.! !


Object subclass: #MQTT5Properties
	instanceVariableNames: 'properties'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

!MQTT5Properties methodsFor: 'initialization'!
initialize
	properties := OrderedCollection new.! !

!MQTT5Properties methodsFor: 'accessing'!
add: aProperty
	self addProperty: aProperty.! !

!MQTT5Properties methodsFor: 'accessing'!
addProperty: aProperty
	properties add: aProperty.! !

!MQTT5Properties methodsFor: 'accessing'!
addPropertyIdentifier: anIdentifier value: aValue
	self addProperty:
		(MQTT5Property
			identifier: anIdentifier
			value: aValue).! !

!MQTT5Properties methodsFor: 'accessing'!
properties
	^ properties.! !

!MQTT5Properties methodsFor: 'accessing'!
propertiesAt: anIdentifier
	^ (properties select: [ :each | each identifier = anIdentifier ]) collect: [ :each | each value ].! !

!MQTT5Properties methodsFor: 'accessing'!
propertyAt: anIdentifier
	properties do: [ :each |
		each identifier = anIdentifier ifTrue: [ ^ each value ] ].
	^ nil.! !

!MQTT5Properties methodsFor: 'accessing'!
size
	^ properties size.! !

!MQTT5Properties methodsFor: 'testing'!
isEmpty
	^ properties isEmpty.! !

!MQTT5Properties methodsFor: 'testing'!
notEmpty
	^ properties notEmpty.! !

!MQTT5Properties methodsFor: 'encoding'!
encodeOn: aStream
	| temp |
	properties isEmpty ifTrue: [
		aStream nextPut: 0.
		^ self ].
	temp := MQTTWriteStream on: (ByteArray new: self estimatedSize).
	properties do: [ :each | each encodeOn: temp ].
	self encodeVariableByteInteger: temp position on: aStream.
	aStream nextPutAll: temp contents.! !

!MQTT5Properties methodsFor: 'encoding'!
encodeVariableByteInteger: value on: aStream
	| v digit |
	v := value.
	[
		digit := v \\ 128.
		v := v // 128.
		v > 0 ifTrue: [ digit := digit bitOr: 128 ].
		aStream nextPut: digit.
		v > 0 ] whileTrue.! !

!MQTT5Properties methodsFor: 'encoding'!
estimatedSize
	| total |
	total := 0.
	properties do: [ :each | total := total + each encodedSize ].
	^ total + 4.! !

!MQTT5Properties methodsFor: 'decoding'!
decode16BitIntegerFrom: aStream
	^ (aStream next << 8) + aStream next.! !

!MQTT5Properties methodsFor: 'decoding'!
decode32BitIntegerFrom: aStream
	^ (aStream next << 24) + (aStream next << 16) + (aStream next << 8) + aStream next.! !

!MQTT5Properties methodsFor: 'decoding'!
decodeBinaryDataFrom: aStream
	| size |
	size := self decode16BitIntegerFrom: aStream.
	^ aStream next: size.! !

!MQTT5Properties methodsFor: 'decoding'!
decodeFrom: aStream
	| length data propStream |
	length := self decodeVariableByteIntegerFrom: aStream.
	length = 0 ifTrue: [ ^ self ].
	data := aStream next: length.
	propStream := ReadStream on: data.
	[ propStream atEnd ] whileFalse: [ self decodePropertyFrom: propStream ].! !

!MQTT5Properties methodsFor: 'decoding'!
decodePropertyFrom: aStream
	| identifier type value |
	identifier := self decodeVariableByteIntegerFrom: aStream.
	type := MQTT5Constants propertyTypeFor: identifier.
	type = #byte ifTrue: [ value := aStream next ].
	type = #twoByteInteger ifTrue: [ value := self decode16BitIntegerFrom: aStream ].
	type = #fourByteInteger ifTrue: [ value := self decode32BitIntegerFrom: aStream ].
	type = #variableByteInteger ifTrue: [ value := self decodeVariableByteIntegerFrom: aStream ].
	type = #utf8String ifTrue: [ value := self decodeStringFrom: aStream ].
	type = #binaryData ifTrue: [ value := self decodeBinaryDataFrom: aStream ].
	type = #utf8StringPair ifTrue: [ value := (self decodeStringFrom: aStream) -> (self decodeStringFrom: aStream) ].
	type ifNil: [ self error: 'Unknown property identifier: ' , identifier printString ].
	properties add: (MQTT5Property identifier: identifier value: value).! !

!MQTT5Properties methodsFor: 'decoding'!
decodeStringFrom: aStream
	| size bytes |
	size := self decode16BitIntegerFrom: aStream.
	bytes := aStream next: size.
	^ UTF8Encoder decode: bytes.! !

!MQTT5Properties methodsFor: 'decoding'!
decodeVariableByteIntegerFrom: aStream
	| digit multiplier value |
	multiplier := 0.
	value := 0.
	[
		digit := aStream next.
		(digit isNil or: [ multiplier > 21 ]) ifTrue: [ self error: 'Invalid variable byte integer' ].
		value := value + ((digit bitAnd: 127) << multiplier).
		multiplier := multiplier + 7.
		(digit bitAnd: 128) > 0 ] whileTrue.
	^ value.! !

!MQTT5Properties methodsFor: 'enumerating'!
do: aBlock
	properties do: aBlock.! !

!MQTT5Properties methodsFor: 'printing'!
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ' (' , properties size printString , ' properties)'.! !

!MQTT5Properties class methodsFor: 'instance creation'!
new
	^ super new initialize.! !


Object subclass: #MQTT5Property
	instanceVariableNames: 'identifier value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

!MQTT5Property methodsFor: 'accessing'!
identifier
	^ identifier.! !

!MQTT5Property methodsFor: 'accessing'!
identifier: anIdentifier value: aValue
	identifier := anIdentifier.
	value := aValue.! !

!MQTT5Property methodsFor: 'accessing'!
value
	^ value.! !

!MQTT5Property methodsFor: 'accessing'!
value: aValue
	value := aValue.! !

!MQTT5Property methodsFor: 'encoding'!
encode16BitInteger: anInteger on: aStream
	aStream
		nextPut: (anInteger >> 8 bitAnd: 255);
		nextPut: (anInteger bitAnd: 255).! !

!MQTT5Property methodsFor: 'encoding'!
encode32BitInteger: anInteger on: aStream
	aStream
		nextPut: (anInteger >> 24 bitAnd: 255);
		nextPut: (anInteger >> 16 bitAnd: 255);
		nextPut: (anInteger >> 8 bitAnd: 255);
		nextPut: (anInteger bitAnd: 255).! !

!MQTT5Property methodsFor: 'encoding'!
encodeBinaryData: aByteArray on: aStream
	self encode16BitInteger: aByteArray size on: aStream.
	aStream nextPutAll: aByteArray.! !

!MQTT5Property methodsFor: 'encoding'!
encodeOn: aStream
	| type |
	self encodeVariableByteInteger: identifier on: aStream.
	type := MQTT5Constants propertyTypeFor: identifier.
	type = #byte ifTrue: [ ^ aStream nextPut: value ].
	type = #twoByteInteger ifTrue: [ ^ self encode16BitInteger: value on: aStream ].
	type = #fourByteInteger ifTrue: [ ^ self encode32BitInteger: value on: aStream ].
	type = #variableByteInteger ifTrue: [ ^ self encodeVariableByteInteger: value on: aStream ].
	type = #utf8String ifTrue: [ ^ self encodeString: value on: aStream ].
	type = #binaryData ifTrue: [ ^ self encodeBinaryData: value on: aStream ].
	type = #utf8StringPair ifTrue: [
		self encodeString: value key on: aStream.
		^ self encodeString: value value on: aStream ].
	self error: 'Unknown property type for identifier: ' , identifier printString.! !

!MQTT5Property methodsFor: 'encoding'!
encodeString: aString on: aStream
	| bytes |
	bytes := UTF8Encoder encode: aString asString.
	self encode16BitInteger: bytes size on: aStream.
	aStream nextPutAll: bytes.! !

!MQTT5Property methodsFor: 'encoding'!
encodeVariableByteInteger: value on: aStream
	| v digit |
	v := value.
	[
		digit := v \\ 128.
		v := v // 128.
		v > 0 ifTrue: [ digit := digit bitOr: 128 ].
		aStream nextPut: digit.
		v > 0 ] whileTrue.! !

!MQTT5Property methodsFor: 'encoding'!
encodedSize
	| idSize type |
	idSize := self variableByteIntegerSize: identifier.
	type := MQTT5Constants propertyTypeFor: identifier.
	type = #byte ifTrue: [ ^ idSize + 1 ].
	type = #twoByteInteger ifTrue: [ ^ idSize + 2 ].
	type = #fourByteInteger ifTrue: [ ^ idSize + 4 ].
	type = #variableByteInteger ifTrue: [ ^ idSize + (self variableByteIntegerSize: value) ].
	type = #utf8String ifTrue: [ ^ idSize + 2 + (UTF8Encoder encode: value asString) size ].
	type = #binaryData ifTrue: [ ^ idSize + 2 + value size ].
	type = #utf8StringPair ifTrue: [
		^ idSize + 2 + (UTF8Encoder encode: value key asString) size + 2 + (UTF8Encoder encode: value value asString) size ].
	^ idSize.! !

!MQTT5Property methodsFor: 'encoding'!
variableByteIntegerSize: anInteger
	anInteger < 128 ifTrue: [ ^ 1 ].
	anInteger < 16384 ifTrue: [ ^ 2 ].
	anInteger < 2097152 ifTrue: [ ^ 3 ].
	^ 4.! !

!MQTT5Property methodsFor: 'printing'!
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ' id: '.
	identifier printOn: aStream.
	aStream nextPutAll: ' value: '.
	value printOn: aStream.! !

!MQTT5Property class methodsFor: 'instance creation'!
identifier: anIdentifier value: aValue
	^ self new
		identifier: anIdentifier
		value: aValue.! !


Object subclass: #MQTT5ReasonCode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

!MQTT5ReasonCode class methodsFor: 'testing'!
isError: anInteger
	^ anInteger >= 128.! !

!MQTT5ReasonCode class methodsFor: 'testing'!
isSuccess: anInteger
	^ anInteger <= 2.! !

!MQTT5ReasonCode class methodsFor: 'accessing'!
descriptionFor: anInteger
	anInteger = 0 ifTrue: [ ^ 'Success' ].
	anInteger = 1 ifTrue: [ ^ 'Granted QoS 1' ].
	anInteger = 2 ifTrue: [ ^ 'Granted QoS 2' ].
	anInteger = 4 ifTrue: [ ^ 'Disconnect with Will Message' ].
	anInteger = 16 ifTrue: [ ^ 'No matching subscribers' ].
	anInteger = 17 ifTrue: [ ^ 'No subscription existed' ].
	anInteger = 24 ifTrue: [ ^ 'Continue authentication' ].
	anInteger = 25 ifTrue: [ ^ 'Re-authenticate' ].
	anInteger = 128 ifTrue: [ ^ 'Unspecified error' ].
	anInteger = 129 ifTrue: [ ^ 'Malformed Packet' ].
	anInteger = 130 ifTrue: [ ^ 'Protocol Error' ].
	anInteger = 131 ifTrue: [ ^ 'Implementation specific error' ].
	anInteger = 132 ifTrue: [ ^ 'Unsupported Protocol Version' ].
	anInteger = 133 ifTrue: [ ^ 'Client Identifier not valid' ].
	anInteger = 134 ifTrue: [ ^ 'Bad User Name or Password' ].
	anInteger = 135 ifTrue: [ ^ 'Not authorized' ].
	anInteger = 136 ifTrue: [ ^ 'Server unavailable' ].
	anInteger = 137 ifTrue: [ ^ 'Server busy' ].
	anInteger = 138 ifTrue: [ ^ 'Banned' ].
	anInteger = 139 ifTrue: [ ^ 'Server shutting down' ].
	anInteger = 140 ifTrue: [ ^ 'Bad authentication method' ].
	anInteger = 141 ifTrue: [ ^ 'Keep Alive timeout' ].
	anInteger = 142 ifTrue: [ ^ 'Session taken over' ].
	anInteger = 143 ifTrue: [ ^ 'Topic Filter invalid' ].
	anInteger = 144 ifTrue: [ ^ 'Topic Name invalid' ].
	anInteger = 145 ifTrue: [ ^ 'Packet Identifier in use' ].
	anInteger = 146 ifTrue: [ ^ 'Packet Identifier not found' ].
	anInteger = 147 ifTrue: [ ^ 'Receive Maximum exceeded' ].
	anInteger = 148 ifTrue: [ ^ 'Topic Alias invalid' ].
	anInteger = 149 ifTrue: [ ^ 'Packet too large' ].
	anInteger = 150 ifTrue: [ ^ 'Message rate too high' ].
	anInteger = 151 ifTrue: [ ^ 'Quota exceeded' ].
	anInteger = 152 ifTrue: [ ^ 'Administrative action' ].
	anInteger = 153 ifTrue: [ ^ 'Payload format invalid' ].
	anInteger = 154 ifTrue: [ ^ 'Retain not supported' ].
	anInteger = 155 ifTrue: [ ^ 'QoS not supported' ].
	anInteger = 156 ifTrue: [ ^ 'Use another server' ].
	anInteger = 157 ifTrue: [ ^ 'Server moved' ].
	anInteger = 158 ifTrue: [ ^ 'Shared Subscriptions not supported' ].
	anInteger = 159 ifTrue: [ ^ 'Connection rate exceeded' ].
	anInteger = 160 ifTrue: [ ^ 'Maximum connect time' ].
	anInteger = 161 ifTrue: [ ^ 'Subscription Identifiers not supported' ].
	anInteger = 162 ifTrue: [ ^ 'Wildcard Subscriptions not supported' ].
	^ 'Unknown reason code: ' , anInteger printString.! !


Object subclass: #MQTT5ServerCapabilities
	instanceVariableNames: 'sessionExpiryInterval receiveMaximum maximumQoS retainAvailable maximumPacketSize assignedClientIdentifier topicAliasMaximum wildcardSubscriptionAvailable subscriptionIdentifiersAvailable sharedSubscriptionAvailable serverKeepAlive responseInformation serverReference authenticationMethod authenticationData'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

!MQTT5ServerCapabilities methodsFor: 'initialization'!
initialize
	super initialize.
	receiveMaximum := 65535.
	maximumQoS := 2.
	retainAvailable := true.
	maximumPacketSize := 268435456.
	topicAliasMaximum := 0.
	wildcardSubscriptionAvailable := true.
	subscriptionIdentifiersAvailable := true.
	sharedSubscriptionAvailable := true.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
assignedClientIdentifier
	^ assignedClientIdentifier.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
assignedClientIdentifier: aString
	assignedClientIdentifier := aString.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
authenticationData
	^ authenticationData.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
authenticationData: aByteArray
	authenticationData := aByteArray.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
authenticationMethod
	^ authenticationMethod.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
authenticationMethod: aString
	authenticationMethod := aString.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
maximumPacketSize
	^ maximumPacketSize.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
maximumPacketSize: anInteger
	maximumPacketSize := anInteger.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
maximumQoS
	^ maximumQoS.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
maximumQoS: anInteger
	maximumQoS := anInteger.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
receiveMaximum
	^ receiveMaximum.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
receiveMaximum: anInteger
	receiveMaximum := anInteger.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
responseInformation
	^ responseInformation.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
responseInformation: aString
	responseInformation := aString.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
retainAvailable
	^ retainAvailable.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
retainAvailable: aBoolean
	retainAvailable := aBoolean.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
serverKeepAlive
	^ serverKeepAlive.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
serverKeepAlive: anInteger
	serverKeepAlive := anInteger.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
serverReference
	^ serverReference.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
serverReference: aString
	serverReference := aString.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
sessionExpiryInterval
	^ sessionExpiryInterval.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
sessionExpiryInterval: anInteger
	sessionExpiryInterval := anInteger.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
sharedSubscriptionAvailable
	^ sharedSubscriptionAvailable.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
sharedSubscriptionAvailable: aBoolean
	sharedSubscriptionAvailable := aBoolean.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
subscriptionIdentifiersAvailable
	^ subscriptionIdentifiersAvailable.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
subscriptionIdentifiersAvailable: aBoolean
	subscriptionIdentifiersAvailable := aBoolean.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
topicAliasMaximum
	^ topicAliasMaximum.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
topicAliasMaximum: anInteger
	topicAliasMaximum := anInteger.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
wildcardSubscriptionAvailable
	^ wildcardSubscriptionAvailable.! !

!MQTT5ServerCapabilities methodsFor: 'accessing'!
wildcardSubscriptionAvailable: aBoolean
	wildcardSubscriptionAvailable := aBoolean.! !

!MQTT5ServerCapabilities methodsFor: 'parsing'!
updateFromConnAckProperties: aProperties
	aProperties ifNil: [ ^ self ].
	aProperties do: [ :prop |
		prop identifier = 17 ifTrue: [ sessionExpiryInterval := prop value ].
		prop identifier = 33 ifTrue: [ receiveMaximum := prop value ].
		prop identifier = 36 ifTrue: [ maximumQoS := prop value ].
		prop identifier = 37 ifTrue: [ retainAvailable := prop value = 1 ].
		prop identifier = 39 ifTrue: [ maximumPacketSize := prop value ].
		prop identifier = 18 ifTrue: [ assignedClientIdentifier := prop value ].
		prop identifier = 34 ifTrue: [ topicAliasMaximum := prop value ].
		prop identifier = 40 ifTrue: [ wildcardSubscriptionAvailable := prop value = 1 ].
		prop identifier = 41 ifTrue: [ subscriptionIdentifiersAvailable := prop value = 1 ].
		prop identifier = 42 ifTrue: [ sharedSubscriptionAvailable := prop value = 1 ].
		prop identifier = 19 ifTrue: [ serverKeepAlive := prop value ].
		prop identifier = 26 ifTrue: [ responseInformation := prop value ].
		prop identifier = 28 ifTrue: [ serverReference := prop value ].
		prop identifier = 21 ifTrue: [ authenticationMethod := prop value ].
		prop identifier = 22 ifTrue: [ authenticationData := prop value ] ].! !

!MQTT5ServerCapabilities class methodsFor: 'instance creation'!
new
	^ super new initialize.! !


Object subclass: #UTF8Encoder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

!UTF8Encoder class methodsFor: 'encoding'!
decode: aByteArray
	| bytes stream i size b1 b2 b3 b4 codePoint |
	aByteArray isString ifTrue: [ ^ aByteArray ].
	aByteArray ifNil: [ ^ '' ].
	bytes := aByteArray asByteArray.
	size := bytes size.
	stream := WriteStream on: (WideString new: size).
	i := 1.
	[ i <= size ] whileTrue: [
		b1 := bytes at: i.
		(b1 < 16r80) ifTrue: [
			codePoint := b1.
			i := i + 1
		] ifFalse: [
			(b1 between: 16rC2 and: 16rDF) ifTrue: [
				(i + 1 <= size
					and: [ ((bytes at: i + 1) between: 16r80 and: 16rBF) ]) ifTrue: [
						b2 := bytes at: i + 1.
						codePoint := ((b1 bitAnd: 16r1F) << 6) + (b2 bitAnd: 16r3F).
						i := i + 2
					] ifFalse: [
						codePoint := 16rFFFD.
						i := i + 1
					]
			] ifFalse: [
				(b1 between: 16rE0 and: 16rEF) ifTrue: [
					(i + 2 <= size
						and: [ ((bytes at: i + 1) between: 16r80 and: 16rBF)
						and: [ (bytes at: i + 2) between: 16r80 and: 16rBF ] ]) ifTrue: [
							b2 := bytes at: i + 1.
							b3 := bytes at: i + 2.
							((b1 = 16rE0 and: [ b2 < 16rA0 ])
								or: [ b1 = 16rED and: [ b2 > 16r9F ] ]) ifTrue: [
									codePoint := 16rFFFD.
									i := i + 1
								] ifFalse: [
									codePoint := ((b1 bitAnd: 16r0F) << 12)
										+ ((b2 bitAnd: 16r3F) << 6)
										+ (b3 bitAnd: 16r3F).
									i := i + 3
								]
						] ifFalse: [
							codePoint := 16rFFFD.
							i := i + 1
						]
				] ifFalse: [
					(b1 between: 16rF0 and: 16rF4) ifTrue: [
						(i + 3 <= size
							and: [ ((bytes at: i + 1) between: 16r80 and: 16rBF)
							and: [ (bytes at: i + 2) between: 16r80 and: 16rBF
							and: [ (bytes at: i + 3) between: 16r80 and: 16rBF ] ] ]) ifTrue: [
								b2 := bytes at: i + 1.
								b3 := bytes at: i + 2.
								b4 := bytes at: i + 3.
								((b1 = 16rF0 and: [ b2 < 16r90 ])
									or: [ b1 = 16rF4 and: [ b2 > 16r8F ] ]) ifTrue: [
										codePoint := 16rFFFD.
										i := i + 1
									] ifFalse: [
										codePoint := ((b1 bitAnd: 16r07) << 18)
											+ ((b2 bitAnd: 16r3F) << 12)
											+ ((b3 bitAnd: 16r3F) << 6)
											+ (b4 bitAnd: 16r3F).
										i := i + 4
									]
							] ifFalse: [
								codePoint := 16rFFFD.
								i := i + 1
							]
					] ifFalse: [
						codePoint := 16rFFFD.
						i := i + 1
					]
				]
			]
		].
		(codePoint between: 16rD800 and: 16rDFFF) ifTrue: [ codePoint := 16rFFFD ].
		(codePoint > 16r10FFFF) ifTrue: [ codePoint := 16rFFFD ].
		stream nextPut: (Character value: codePoint) ].
	^ stream contents.! !

!UTF8Encoder class methodsFor: 'encoding'!
encode: aString
	| str stream cp |
	aString ifNil: [ ^ ByteArray new ].
	str := aString asString.
	stream := WriteStream on: (ByteArray new: (str size * 4 max: 4)).
	str do: [ :ch |
		cp := (ch respondsTo: #codePoint)
			ifTrue: [ ch codePoint ]
			ifFalse: [ ch asInteger ].
		(cp between: 16rD800 and: 16rDFFF) ifTrue: [ cp := 16rFFFD ].
		cp <= 16r7F ifTrue: [
			stream nextPut: cp
		] ifFalse: [
			cp <= 16r7FF ifTrue: [
				stream nextPut: (16rC0 + (cp >> 6)).
				stream nextPut: (16r80 + (cp bitAnd: 16r3F))
			] ifFalse: [
				cp <= 16rFFFF ifTrue: [
					stream nextPut: (16rE0 + (cp >> 12)).
					stream nextPut: (16r80 + ((cp >> 6) bitAnd: 16r3F)).
					stream nextPut: (16r80 + (cp bitAnd: 16r3F))
				] ifFalse: [
					cp <= 16r10FFFF ifTrue: [
						stream nextPut: (16rF0 + (cp >> 18)).
						stream nextPut: (16r80 + ((cp >> 12) bitAnd: 16r3F)).
						stream nextPut: (16r80 + ((cp >> 6) bitAnd: 16r3F)).
						stream nextPut: (16r80 + (cp bitAnd: 16r3F))
					] ifFalse: [
						stream nextPut: 16rEF;
							nextPut: 16rBF;
							nextPut: 16rBD
					]
				]
			]
		] ].
	^ stream contents copyFrom: 1 to: stream position.! !


Object subclass: #MQTTStatistics
	instanceVariableNames: 'pin pout poutMaxSize poutMinSize pinMaxSize pinMinSize pinBytes poutBytes pinTypeCount poutTypeCount errorTypes connections disconnect connected connectionTypes servers subscriptions timeOut killSocket sentPackets queuedPackets unsubscribe badConnections missingType resendType'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

!MQTTStatistics methodsFor: 'initialization'!
initialize
	super initialize.
	pin := 0.
	pout := 0.
	pinBytes := 0.
	poutBytes := 0.
	pinMaxSize := 0.
	pinMinSize := 99999999.
	poutMaxSize := 0.
	poutMinSize := 99999999.
	pinTypeCount := Dictionary new.
	poutTypeCount := Dictionary new.
	errorTypes := Dictionary new.
	connections := 0.
	disconnect := 0.
	connected := false.
	connectionTypes := Dictionary new.
	servers := Dictionary new.
	subscriptions := 0.
	timeOut := 0.
	killSocket := 0.
	sentPackets := 0.
	queuedPackets := 0.
	unsubscribe := 0.
	badConnections := 0.
	missingType := 0.
	resendType := 0.! !

!MQTTStatistics methodsFor: 'accessing'!
pin
	^ pin.! !

!MQTTStatistics methodsFor: 'accessing'!
pinBytes
	^ pinBytes.! !

!MQTTStatistics methodsFor: 'accessing'!
pout
	^ pout.! !

!MQTTStatistics methodsFor: 'accessing'!
poutBytes
	^ poutBytes.! !

!MQTTStatistics methodsFor: 'updating'!
recordConnection
	connections := connections + 1.
	connected := true.! !

!MQTTStatistics methodsFor: 'updating'!
recordDisconnection
	disconnect := disconnect + 1.
	connected := false.! !

!MQTTStatistics methodsFor: 'updating'!
recordError: aSymbol
	errorTypes
		at: aSymbol
		put: ((errorTypes at: aSymbol ifAbsent: [ 0 ]) + 1).! !

!MQTTStatistics methodsFor: 'updating'!
recordIncomingPacket: aPacket size: aSize
	pin := pin + 1.
	pinBytes := pinBytes + aSize.
	pinMaxSize := pinMaxSize max: aSize.
	pinMinSize := pinMinSize min: aSize.
	pinTypeCount
		at: aPacket class name
		put: ((pinTypeCount at: aPacket class name ifAbsent: [ 0 ]) + 1).! !

!MQTTStatistics methodsFor: 'updating'!
recordOutgoingPacket: aPacket size: aSize
	pout := pout + 1.
	poutBytes := poutBytes + aSize.
	poutMaxSize := poutMaxSize max: aSize.
	poutMinSize := poutMinSize min: aSize.
	poutTypeCount
		at: aPacket class name
		put: ((poutTypeCount at: aPacket class name ifAbsent: [ 0 ]) + 1).! !

SystemOrganization addCategory: #'MQTT5-Squeak'!

Object subclass: #MQTTPacket
	instanceVariableNames: ''
	classVariableNames: 'MQTTProtocolLevel311 MQTTProtocolLevel50 PacketTypes'
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPacket subclass: #MQTTPacketAuth
	instanceVariableNames: 'remainingLength reasonCode properties'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPacket subclass: #MQTTPacketDisconnect
	instanceVariableNames: 'reasonCode properties'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPacket subclass: #MQTTPacketPingReq
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPacket subclass: #MQTTPacketPingResp
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPacket subclass: #MQTTPacketVariableHeaded
	instanceVariableNames: 'remainingLength msgID properties'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPacketVariableHeaded subclass: #MQTTPacketConnAck
	instanceVariableNames: 'sessionPresent byte1 byte2 reasonCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPacketVariableHeaded subclass: #MQTTPacketPubAck
	instanceVariableNames: 'reasonCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPacketVariableHeaded subclass: #MQTTPacketPubComp
	instanceVariableNames: 'reasonCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPacketVariableHeaded subclass: #MQTTPacketPubRec
	instanceVariableNames: 'originalPacket reasonCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPacketVariableHeaded subclass: #MQTTPacketPubRel
	instanceVariableNames: 'reasonCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPacketVariableHeaded subclass: #MQTTPacketUnsubAck
	instanceVariableNames: 'reasonCodes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPacketVariableHeaded subclass: #MQTTPacketVariableHeadedWithPayload
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPacketVariableHeadedWithPayload subclass: #MQTTPacketConnect
	instanceVariableNames: 'user password keepAliveTime willMessage willTopic willQos willFlag willRetainFlag cleanSessionFlag clientID returnCode protocolVersion willProperties sessionExpiryInterval receiveMaximum maximumPacketSize topicAliasMaximum requestResponseInfo requestProblemInfo authenticationMethod authenticationData'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPacketVariableHeadedWithPayload subclass: #MQTTPacketPublish
	instanceVariableNames: 'topic message duplicate qos retain topicAlias payloadFormatIndicator messageExpiryInterval responseTopic correlationData subscriptionIdentifiers contentType'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPacketVariableHeadedWithPayload subclass: #MQTTPacketSubAck
	instanceVariableNames: 'returnCodes reasonCodes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPacketVariableHeadedWithPayload subclass: #MQTTPacketSubscribe
	instanceVariableNames: 'payloadDict subscriptionIdentifier'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPacketVariableHeadedWithPayload subclass: #MQTTPacketUnsubscribe
	instanceVariableNames: 'topics'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!


!MQTTPacket methodsFor: 'mqtt'!
badConnectAcknowledgeError
	self debugLog: 'badConnectAcknowledgeError'.
	MQTTConnectionException signal: 'badConnectAcknowledgeError'.!
!
badFixedHeaderError
	self debugLog: 'badFixedHeaderError'.
	^ MQTTBadHeaderException signal.!
!
badQosError
	self debugLog: 'badQosError'.
	MQTTBadHeaderException signal: 'badQosError'.!
!
badTopicError
	self debugLog: 'badTopicError'.
	MQTTBadTopicException signal: 'badTopicError'.!
!
badTopicListError
	self debugLog: 'badTopicListError'.
	MQTTBadTopicException signal: 'badTopicListError'.!
!
connectionRefusedError: arg1
	self debugLog: 'connectionRefusedError:'.
	^ MQTTCONNECTException connectReturnCode: arg1.!
!
debugLog: arg1
	self class debugLog: arg1.!
!
encodedLengthError
	self debugLog: 'encodedLengthError'.
	MQTTBadHeaderException signal: 'encodedLengthError'.!
!
evaluateFor: arg1
	^ self subclassResponsibility.!
!

!MQTTPacket methodsFor: 'encoding'!
decode16BitIntegerFrom: arg1
	^ arg1 next << 8 + arg1 next.!
!
decode32BitIntegerFrom: arg1
	^ arg1 next << 24 + (arg1 next << 16) + (arg1 next << 8) + arg1 next.!
!
decodeBinaryDataFrom: arg1
	| temp2 |
	temp2 := self decode16BitIntegerFrom: arg1.
	^ arg1 next: temp2.!
!
decodeFixedHeaderFrom: arg1
	| temp2 |
	temp2 := arg1 next.
	temp2 = self fixedHeader ifFalse: [ self badFixedHeaderError ].
	self decodeLengthFrom: arg1.!
!
decodeFrom: arg1
	self decodeFixedHeaderFrom: arg1.!
!
decodeLengthFrom: arg1
	| temp2 |
	temp2 := arg1 next.
	temp2 > 0 ifTrue: [ self encodedLengthError ].!
!
decodeStringFrom: arg1
	| temp2 temp3 |
	temp2 := self decode16BitIntegerFrom: arg1.
	temp3 := arg1 next: temp2.
	^ UTF8Encoder decode: temp3.!
!
decodeVariableByteIntegerFrom: arg1
	| temp2 temp3 temp4 |
	temp3 := 0.
	temp4 := 0.
	[
	temp2 := arg1 next.
	(temp2 isNil or: [ temp3 > 21 ]) ifTrue: [ self encodedLengthError ].
	temp4 := temp4 + ((temp2 bitAnd: 127) << temp3).
	temp3 := temp3 + 7.
	(temp2 bitAnd: 128) > 0 ] whileTrue.
	^ temp4.!
!
encode16BitInteger: arg1 on: arg2
	arg2
		nextPut: arg1 >> 8;
		nextPut: (arg1 bitAnd: 255).!
!
encode32BitInteger: arg1 on: arg2
	arg2
		nextPut: (arg1 >> 24 bitAnd: 255);
		nextPut: (arg1 >> 16 bitAnd: 255);
		nextPut: (arg1 >> 8 bitAnd: 255);
		nextPut: (arg1 bitAnd: 255).!
!
encodeBinaryData: arg1 on: arg2
	self
		encode16BitInteger: arg1 size
		on: arg2.
	arg2 nextPutAll: arg1.!
!
encodeOn: arg1
	arg1
		nextPut: self fixedHeader;
		nextPut: 0;
		flush.
	^ arg1.!
!
encodeString: arg1 on: arg2
	| temp3 |
	temp3 := UTF8Encoder encode: arg1 asString.
	self
		encode16BitInteger: temp3 size
		on: arg2.
	arg2 nextPutAll: temp3.!
!
encodeVariableByteInteger: arg1 on: arg2
	self encodeVariableLength: arg1 on: arg2.!
!
encodeVariableLength: arg1 on: arg2
	| temp3 temp4 |
	temp3 := arg1.
	[
	temp4 := temp3 \\ 128.
	temp3 := temp3 // 128.
	temp3 > 0 ifTrue: [ temp4 := temp4 bitOr: 128 ].
	arg2 nextPut: temp4.
	temp3 > 0 ] whileTrue.!
!
readFrom: arg1
	self decodeFrom: arg1.!
!

!MQTTPacket methodsFor: 'accessing'!
fixedHeader
	^ self packetType << 4 bitOr: self fixedHeaderFlags.!
!
fixedHeaderFlags
	^ 0.!
!
packetType
	^ nil.!
!

!MQTTPacket class methodsFor: 'class initialization'!
initialize
	PacketTypes := OrderedCollection new
		add: MQTTPacketConnect;
		add: MQTTPacketConnAck;
		add: MQTTPacketPublish;
		add: MQTTPacketPubAck;
		add: MQTTPacketPubRec;
		add: MQTTPacketPubRel;
		add: MQTTPacketPubComp;
		add: MQTTPacketSubscribe;
		add: MQTTPacketSubAck;
		add: MQTTPacketUnsubscribe;
		add: MQTTPacketUnsubAck;
		add: MQTTPacketPingReq;
		add: MQTTPacketPingResp;
		add: MQTTPacketDisconnect;
		add: MQTTPacketAuth;
		yourself.
	MQTTProtocolLevel311 := 4.
	MQTTProtocolLevel50 := 5.!
!

!MQTTPacket class methodsFor: 'accessing'!
protocolLevel311
	^ MQTTProtocolLevel311.!
!
protocolLevel50
	^ MQTTProtocolLevel50.!
!

!MQTTPacket class methodsFor: 'mqtt'!
badPacketTypeError
	self debugLog: 'badPacketTypeError'.
	MQTTBadPacketTypeException signal.!
!
debugLog: arg1
	MQTTClientInterface
		debugLog: arg1
		tag: ' PP '
		str2: ''.!
!
emptyStreamError
	^ MQTTEmptyStreamErrorException signal: 'MQTT no bytes available to be read'.!
!
readFrom: arg1
	| temp2 |
	temp2 := [ arg1 peek ]
		on: ConnectionTimedOut
		do: [ :argm0_3 |
			self emptyStreamError ].
	temp2 ifNil: [ ^ self emptyStreamError ].
	^ (PacketTypes
		at: temp2 >> 4
		ifAbsent: [ self badPacketTypeError ]) new readFrom: arg1.!
!

!MQTTPacketAuth methodsFor: 'initialization'!
initialize
	super initialize.
	reasonCode := 0.!
!

!MQTTPacketAuth methodsFor: 'accessing'!
authenticationData
	properties ifNil: [ ^ nil ].
	^ properties propertyAt: 22.!
!
authenticationData: arg1
	properties ifNil: [ properties := MQTT5Properties new ].
	properties addProperty:
		(MQTT5Property
			identifier: 22
			value: arg1).!
!
authenticationMethod
	properties ifNil: [ ^ nil ].
	^ properties propertyAt: 21.!
!
authenticationMethod: arg1
	properties ifNil: [ properties := MQTT5Properties new ].
	properties addProperty:
		(MQTT5Property
			identifier: 21
			value: arg1).!
!
packetType
	^ 15.!
!
properties
	^ properties.!
!
properties: arg1
	properties := arg1.!
!
reasonCode
	^ reasonCode.!
!
reasonCode: arg1
	reasonCode := arg1.!
!
reasonString
	properties ifNil: [ ^ nil ].
	^ properties propertyAt: 31.!
!
reasonString: arg1
	properties ifNil: [ properties := MQTT5Properties new ].
	properties addProperty:
		(MQTT5Property
			identifier: 31
			value: arg1).!
!

!MQTTPacketAuth methodsFor: 'mqtt'!
evaluateFor: arg1
	arg1 handleAuthPacket: self.!
!

!MQTTPacketAuth methodsFor: 'testing'!
isContinueAuthentication
	^ reasonCode = 24.!
!
isReauthenticate
	^ reasonCode = 25.!
!
isSuccess
	^ reasonCode = 0.!
!

!MQTTPacketAuth methodsFor: 'encoding'!
decodeBinaryDataFrom: arg1
	| temp2 |
	temp2 := (arg1 next bitShift: 8) bitOr: arg1 next.
	^ arg1 next: temp2.!
!
decodeFrom: arg1 remainingLength: arg2
	remainingLength := arg2.
	arg2 = 0 ifTrue: [
		reasonCode := 0.
		^ self ].
	reasonCode := arg1 next.
	arg2 = 1 ifTrue: [ ^ self ].
	self decodePropertiesFrom: arg1.!
!
decodePropertiesFrom: arg1
	| temp2 temp3 temp4 |
	temp2 := self decodeVariableByteIntegerFrom: arg1.
	temp2 = 0 ifTrue: [ ^ self ].
	temp3 := arg1 position + temp2.
	properties := MQTT5Properties new.
	[ arg1 position < temp3 ] whileTrue: [
		temp4 := arg1 next.
		temp4 = 21
			ifTrue: [
				properties addProperty:
					(MQTT5Property
						identifier: temp4
						value: (self decodeStringFrom: arg1)) ]
			ifFalse: [
				temp4 = 22
					ifTrue: [
						properties addProperty:
							(MQTT5Property
								identifier: temp4
								value: (self decodeBinaryDataFrom: arg1)) ]
					ifFalse: [
						temp4 = 31
							ifTrue: [
								properties addProperty:
									(MQTT5Property
										identifier: temp4
										value: (self decodeStringFrom: arg1)) ]
							ifFalse: [
								temp4 = 38 ifTrue: [
									properties addProperty:
										(MQTT5Property
											identifier: temp4
											value:
												(Array
													with: (self decodeStringFrom: arg1)
													with: (self decodeStringFrom: arg1))) ]]]]].!
!
decodeStringFrom: arg1
	| temp2 temp3 |
	temp2 := (arg1 next bitShift: 8) bitOr: arg1 next.
	temp3 := arg1 next: temp2.
	^ UTF8Encoder decode: temp3.!
!
decodeVariableByteIntegerFrom: arg1
	| temp2 temp3 temp4 |
	temp2 := 1.
	temp3 := 0.
	[
	temp4 := arg1 next.
	temp3 := temp3 + ((temp4 bitAnd: 127) * temp2).
	temp2 := temp2 * 128.
	(temp4 bitAnd: 128) ~= 0 ] whileTrue.
	^ temp3.!
!
encodeOn: arg1
	| temp2 |
	temp2 := MQTTWriteStream on: (ByteArray new: 256).
	temp2 nextPut: (reasonCode ifNil: [ 0 ]).
	(properties notNil and: [ properties isEmpty not ])
		ifTrue: [ properties encodeOn: temp2 ]
		ifFalse: [ temp2 nextPut: 0 ].
	arg1 nextPut: (self packetType bitShift: 4).
	self
		encodeVariableLength: temp2 position
		on: arg1.
	arg1 nextPutAll: temp2 contents.!
!
encodeVariableLength: arg1 on: arg2
	| temp3 temp4 |
	temp3 := arg1.
	[
	temp4 := temp3 \\ 128.
	temp3 := temp3 // 128.
	temp3 > 0 ifTrue: [ temp4 := temp4 bitOr: 128 ].
	arg2 nextPut: temp4.
	temp3 > 0 ] whileTrue.!
!

!MQTTPacketDisconnect methodsFor: 'initialization'!
initialize
	super initialize.
	reasonCode := 0.!
!

!MQTTPacketDisconnect methodsFor: 'accessing'!
packetType
	^ 14.!
!
properties
	^ properties.!
!
properties: arg1
	properties := arg1.!
!
reasonCode
	^ reasonCode.!
!
reasonCode: arg1
	reasonCode := arg1.!
!
reasonString
	properties ifNil: [ ^ nil ].
	^ properties propertyAt: 31.!
!
reasonString: arg1
	properties ifNil: [ properties := MQTT5Properties new ].
	properties addProperty:
		(MQTT5Property
			identifier: 31
			value: arg1).!
!
serverReference
	properties ifNil: [ ^ nil ].
	^ properties propertyAt: 28.!
!
sessionExpiryInterval
	properties ifNil: [ ^ nil ].
	^ properties propertyAt: 17.!
!
sessionExpiryInterval: arg1
	properties ifNil: [ properties := MQTT5Properties new ].
	properties addProperty:
		(MQTT5Property
			identifier: 17
			value: arg1).!
!

!MQTTPacketDisconnect methodsFor: 'mqtt'!
evaluateFor: arg1
	arg1 handleDisconnectPacket: self.!
!

!MQTTPacketDisconnect methodsFor: 'encoding'!
decode32BitIntegerFrom: arg1
	^ ((arg1 next bitShift: 24) bitOr: (arg1 next bitShift: 16)) bitOr: ((arg1 next bitShift: 8) bitOr: arg1 next).!
!
decodeFrom: arg1 remainingLength: arg2
	arg2 = 0 ifTrue: [
		reasonCode := 0.
		^ self ].
	reasonCode := arg1 next.
	arg2 = 1 ifTrue: [ ^ self ].
	self decodePropertiesFrom: arg1.!
!
decodePropertiesFrom: arg1
	| temp2 temp3 temp4 |
	temp2 := self decodeVariableByteIntegerFrom: arg1.
	temp2 = 0 ifTrue: [ ^ self ].
	temp3 := arg1 position + temp2.
	properties := MQTT5Properties new.
	[ arg1 position < temp3 ] whileTrue: [
		temp4 := arg1 next.
		temp4 = 17
			ifTrue: [
				properties addProperty:
					(MQTT5Property
						identifier: temp4
						value: (self decode32BitIntegerFrom: arg1)) ]
			ifFalse: [
				temp4 = 31
					ifTrue: [
						properties addProperty:
							(MQTT5Property
								identifier: temp4
								value: (self decodeStringFrom: arg1)) ]
					ifFalse: [
						temp4 = 28
							ifTrue: [
								properties addProperty:
									(MQTT5Property
										identifier: temp4
										value: (self decodeStringFrom: arg1)) ]
							ifFalse: [
								temp4 = 38 ifTrue: [
									properties addProperty:
										(MQTT5Property
											identifier: temp4
											value:
												(Array
													with: (self decodeStringFrom: arg1)
													with: (self decodeStringFrom: arg1))) ]]]]].!
!
decodeStringFrom: arg1
	| temp2 temp3 |
	temp2 := (arg1 next bitShift: 8) bitOr: arg1 next.
	temp3 := arg1 next: temp2.
	^ UTF8Encoder decode: temp3.!
!
decodeVariableByteIntegerFrom: arg1
	| temp2 temp3 temp4 |
	temp2 := 1.
	temp3 := 0.
	[
	temp4 := arg1 next.
	temp3 := temp3 + ((temp4 bitAnd: 127) * temp2).
	temp2 := temp2 * 128.
	(temp4 bitAnd: 128) ~= 0 ] whileTrue.
	^ temp3.!
!
encodeOn: arg1
	| temp2 |
	temp2 := MQTTWriteStream on: (ByteArray new: 64).
	((reasonCode isNil or: [ reasonCode = 0 ]) and: [
		properties isNil or: [ properties isEmpty ]]) ifTrue: [
		arg1 nextPut: (self packetType bitShift: 4).
		arg1 nextPut: 0.
		^ self ].
	temp2 nextPut: (reasonCode ifNil: [ 0 ]).
	(properties notNil and: [ properties isEmpty not ])
		ifTrue: [ properties encodeOn: temp2 ]
		ifFalse: [ temp2 nextPut: 0 ].
	arg1 nextPut: (self packetType bitShift: 4).
	self
		encodeVariableLength: temp2 position
		on: arg1.
	arg1 nextPutAll: temp2 contents.!
!
encodeVariableLength: arg1 on: arg2
	| temp3 temp4 |
	temp3 := arg1.
	[
	temp4 := temp3 \\ 128.
	temp3 := temp3 // 128.
	temp3 > 0 ifTrue: [ temp4 := temp4 bitOr: 128 ].
	arg2 nextPut: temp4.
	temp3 > 0 ] whileTrue.!
!

!MQTTPacketPingReq methodsFor: 'accessing'!
packetType
	^ 12.!
!

!MQTTPacketPingReq methodsFor: 'mqtt'!
evaluateFor: arg1
	arg1 handlePingReqPacket: self.!
!
pendingJob
	^ MQTTPendingPingJob for: self.!
!

!MQTTPacketPingReq methodsFor: 'encoding'!
encodeOn: arg1
	arg1 nextPut: (self packetType bitShift: 4).
	arg1 nextPut: 0.!
!

!MQTTPacketPingResp methodsFor: 'accessing'!
packetType
	^ 13.!
!

!MQTTPacketPingResp methodsFor: 'mqtt'!
evaluateFor: arg1
	arg1 handlePingRespPacket: self.!
!

!MQTTPacketPingResp methodsFor: 'encoding'!
encodeOn: arg1
	arg1 nextPut: (self packetType bitShift: 4).
	arg1 nextPut: 0.!
!

!MQTTPacketVariableHeaded methodsFor: 'initialization'!
initialize
	properties := MQTT5Properties new.!
!

!MQTTPacketVariableHeaded methodsFor: 'encoding'!
decodeByteArrayFrom: arg1
	| temp2 temp3 |
	temp2 := self decode16BitIntegerFrom: arg1.
	temp3 := arg1 next: temp2.
	^ temp3.!
!
decodeFrom: arg1
	super decodeFrom: arg1.
	self decodeVariableHeaderFrom: arg1.!
!
decodeLengthFrom: arg1
	| temp2 temp3 temp4 |
	temp3 := 0.
	temp4 := 0.
	[
	temp2 := arg1 next.
	(temp2 isNil or: [ temp3 > 21 ]) ifTrue: [ self encodedLengthError ].
	temp4 := temp4 + ((temp2 bitAnd: 127) << temp3).
	temp3 := temp3 + 7.
	(temp2 bitAnd: 128) > 0 ] whileTrue.
	remainingLength := temp4.!
!
decodePropertiesFrom: arg1
	properties := MQTT5Properties new.
	properties decodeFrom: arg1.!
!
decodeStringFrom: arg1
	| temp2 temp3 |
	temp2 := self decode16BitIntegerFrom: arg1.
	temp3 := arg1 next: temp2.
	^ UTF8Encoder decode: temp3.!
!
decodeVariableHeaderFrom: arg1
	msgID := self decode16BitIntegerFrom: arg1.!
!
encodeFixedHeaderOn: arg1
	arg1 nextPut: self fixedHeader.
	self encodeLengthOn: arg1.!
!
encodeLengthOn: arg1
	| temp2 temp3 |
	temp2 := remainingLength.
	[
	temp3 := temp2 \\ 128.
	temp2 := temp2 // 128.
	temp2 > 0 ifTrue: [ temp3 := temp3 bitOr: 128 ].
	arg1 nextPut: temp3.
	temp2 > 0 ] whileTrue.!
!
encodeOn: arg1
	| temp2 |
	temp2 := MQTTWriteStream on: (ByteArray new: self guessEncodedLength).
	self encodeVariableHeaderOn: temp2.
	self encodePayloadOn: temp2.
	remainingLength := temp2 position.
	self encodeFixedHeaderOn: arg1.
	arg1
		next: temp2 position
		putAll: temp2 contents
		startingAt: 1.
	arg1 flush.
	^ arg1.!
!
encodePayloadOn: arg1
	^ self.!
!
encodePropertiesOn: arg1
	properties ifNil: [ properties := MQTT5Properties new ].
	properties encodeOn: arg1.!
!
encodeVariableHeaderOn: arg1
	self
		encode16BitInteger: msgID
		on: arg1.!
!

!MQTTPacketVariableHeaded methodsFor: 'accessing'!
guessEncodedLength
	^ 100.!
!
packetID
	^ msgID.!
!
packetID: arg1
	msgID := arg1.!
!
properties
	^ properties ifNil: [ properties := MQTT5Properties new ].!
!
properties: arg1
	properties := arg1.!
!
remainingLength
	^ remainingLength.!
!

!MQTTPacketVariableHeaded methodsFor: 'printing'!
printOn: arg1
	super printOn: arg1.
	msgID ifNotNil: [
		arg1 nextPutAll: ' msgID: '.
		msgID printOn: arg1 ].!
!

!MQTTPacketConnAck methodsFor: 'initialization'!
initialize
	super initialize.
	reasonCode := 0.!
!

!MQTTPacketConnAck methodsFor: 'accessing'!
byte1
	^ byte1.!
!
byte2
	^ byte2.!
!
byte2: arg1
	byte2 := arg1.!
!
packetType
	^ 2.!
!
reasonCode
	^ reasonCode.!
!
reasonCode: arg1
	reasonCode := arg1.!
!
returnCode
	^ byte2.!
!
returnCode: arg1
	byte2 := arg1.!
!
sessionPresent
	^ sessionPresent.!
!
sessionPresent: arg1
	sessionPresent := arg1.!
!

!MQTTPacketConnAck methodsFor: 'encoding'!
decodeVariableHeaderFrom: arg1
	byte1 := arg1 next.
	(byte1 anyMask: 254) ifTrue: [ ^ self badConnectAcknowledgeError ].
	sessionPresent := byte1 allMask: 1.
	byte2 := arg1 next.
	reasonCode := byte2.
	remainingLength > 2 ifTrue: [ self decodePropertiesFrom: arg1 ].!
!
encodeVariableHeaderOn: arg1
	byte1 := sessionPresent asBit.
	arg1
		nextPut: sessionPresent asBit;
		nextPut: (reasonCode ifNil: [ byte2 ifNil: [ 0 ]]).!
!

!MQTTPacketConnAck methodsFor: 'mqtt'!
evaluateFor: arg1
	arg1 handleConnAckPacket: self.!
!

!MQTTPacketPubAck methodsFor: 'initialization'!
initialize
	super initialize.
	reasonCode := 0.!
!

!MQTTPacketPubAck methodsFor: 'mqtt'!
evaluateFor: arg1
	arg1 handlePubAckPacket: self.!
!

!MQTTPacketPubAck methodsFor: 'accessing'!
packetType
	^ 4.!
!
reasonCode
	^ reasonCode.!
!
reasonCode: arg1
	reasonCode := arg1.!
!

!MQTTPacketPubAck methodsFor: 'encoding'!
decodeVariableHeaderFrom: arg1
	msgID := self decode16BitIntegerFrom: arg1.
	remainingLength >= 3 ifTrue: [ reasonCode := arg1 next ].
	remainingLength > 3 ifTrue: [ self decodePropertiesFrom: arg1 ].!
!
encodeVariableHeaderOn: arg1
	self
		encode16BitInteger: msgID
		on: arg1.
	(reasonCode notNil and: [ reasonCode ~= 0 ]) ifTrue: [
		arg1 nextPut: reasonCode.
		self encodePropertiesOn: arg1 ].!
!

!MQTTPacketPubComp methodsFor: 'initialization'!
initialize
	super initialize.
	reasonCode := 0.!
!

!MQTTPacketPubComp methodsFor: 'mqtt'!
evaluateFor: arg1
	arg1 handlePubCompPacket: self.!
!

!MQTTPacketPubComp methodsFor: 'accessing'!
packetType
	^ 7.!
!
reasonCode
	^ reasonCode.!
!
reasonCode: arg1
	reasonCode := arg1.!
!

!MQTTPacketPubComp methodsFor: 'encoding'!
decodeVariableHeaderFrom: arg1
	msgID := self decode16BitIntegerFrom: arg1.
	remainingLength >= 3 ifTrue: [ reasonCode := arg1 next ].
	remainingLength > 3 ifTrue: [ self decodePropertiesFrom: arg1 ].!
!
encodeVariableHeaderOn: arg1
	self
		encode16BitInteger: msgID
		on: arg1.
	(reasonCode notNil and: [ reasonCode ~= 0 ]) ifTrue: [
		arg1 nextPut: reasonCode.
		self encodePropertiesOn: arg1 ].!
!

!MQTTPacketPubRec methodsFor: 'initialization'!
initialize
	super initialize.
	reasonCode := 0.!
!

!MQTTPacketPubRec methodsFor: 'mqtt'!
acknowledgement
	^ MQTTPacketPubRel new packetID: msgID.!
!
evaluateFor: arg1
	arg1 handlePubRecPacket: self.!
!

!MQTTPacketPubRec methodsFor: 'accessing'!
originalPacket
	^ originalPacket.!
!
originalPacket: arg1
	originalPacket := arg1.!
!
packetType
	^ 5.!
!
reasonCode
	^ reasonCode.!
!
reasonCode: arg1
	reasonCode := arg1.!
!

!MQTTPacketPubRec methodsFor: 'encoding'!
decodeVariableHeaderFrom: arg1
	msgID := self decode16BitIntegerFrom: arg1.
	remainingLength >= 3 ifTrue: [ reasonCode := arg1 next ].
	remainingLength > 3 ifTrue: [ self decodePropertiesFrom: arg1 ].!
!
encodeVariableHeaderOn: arg1
	self
		encode16BitInteger: msgID
		on: arg1.
	(reasonCode notNil and: [ reasonCode ~= 0 ]) ifTrue: [
		arg1 nextPut: reasonCode.
		self encodePropertiesOn: arg1 ].!
!

!MQTTPacketPubRel methodsFor: 'initialization'!
initialize
	super initialize.
	reasonCode := 0.!
!

!MQTTPacketPubRel methodsFor: 'mqtt'!
acknowledgement
	^ MQTTPacketPubComp new packetID: msgID.!
!
evaluateFor: arg1
	arg1 handlePubRelPacket: self.!
!
pendingJob
	^ MQTTPendingPubCompJob for: self.!
!

!MQTTPacketPubRel methodsFor: 'accessing'!
fixedHeaderFlags
	^ 2.!
!
packetType
	^ 6.!
!
reasonCode
	^ reasonCode.!
!
reasonCode: arg1
	reasonCode := arg1.!
!

!MQTTPacketPubRel methodsFor: 'encoding'!
decodeVariableHeaderFrom: arg1
	msgID := self decode16BitIntegerFrom: arg1.
	remainingLength >= 3 ifTrue: [ reasonCode := arg1 next ].
	remainingLength > 3 ifTrue: [ self decodePropertiesFrom: arg1 ].!
!
encodeVariableHeaderOn: arg1
	self
		encode16BitInteger: msgID
		on: arg1.
	(reasonCode notNil and: [ reasonCode ~= 0 ]) ifTrue: [
		arg1 nextPut: reasonCode.
		self encodePropertiesOn: arg1 ].!
!

!MQTTPacketUnsubAck methodsFor: 'initialization'!
initialize
	super initialize.
	reasonCodes := OrderedCollection new.!
!

!MQTTPacketUnsubAck methodsFor: 'accessing'!
packetType
	^ 11.!
!
reasonCodes
	^ reasonCodes.!
!
reasonCodes: arg1
	reasonCodes := arg1.!
!

!MQTTPacketUnsubAck methodsFor: 'mqtt'!
evaluateFor: arg1
	arg1 handleUnsubAckPacket: self.!
!

!MQTTPacketUnsubAck methodsFor: 'testing'!
hasFailures
	^ reasonCodes anySatisfy: [ :argm0_1 |
		argm0_1 >= 128 ].!
!
isSuccess
	^ reasonCodes allSatisfy: [ :argm0_1 |
		argm0_1 = 0 ].!
!

!MQTTPacketUnsubAck methodsFor: 'encoding'!
decodePayloadFrom: arg1 length: arg2
	reasonCodes := OrderedCollection new.
	arg2 timesRepeat: [ reasonCodes add: arg1 next ].!
!
decodePropertiesFrom: arg1
	| temp2 temp3 temp4 |
	temp2 := self decodeVariableByteIntegerFrom: arg1.
	temp2 = 0 ifTrue: [ ^ self ].
	temp3 := arg1 position + temp2.
	properties := MQTT5Properties new.
	[ arg1 position < temp3 ] whileTrue: [
		temp4 := arg1 next.
		temp4 = 31
			ifTrue: [
				properties addProperty:
					(MQTT5Property
						identifier: temp4
						value: (self decodeStringFrom: arg1)) ]
			ifFalse: [
				temp4 = 38 ifTrue: [
					properties addProperty:
						(MQTT5Property
							identifier: temp4
							value:
								(Array
									with: (self decodeStringFrom: arg1)
									with: (self decodeStringFrom: arg1))) ]]].!
!
decodeVariableHeaderFrom: arg1
	msgID := self decode16BitIntegerFrom: arg1.
	self decodePropertiesFrom: arg1.!
!
encodePayloadOn: arg1
	reasonCodes do: [ :argm0_2 |
		arg1 nextPut: argm0_2 ].!
!
encodePropertiesOn: arg1
	(properties isNil or: [ properties isEmpty ]) ifTrue: [
		arg1 nextPut: 0.
		^ self ].
	properties encodeOn: arg1.!
!
encodeVariableHeaderOn: arg1
	self
		encode16BitInteger: msgID
		on: arg1.
	self encodePropertiesOn: arg1.!
!

!MQTTPacketVariableHeadedWithPayload methodsFor: 'encoding'!
decodeFrom: arg1
	super decodeFrom: arg1.
	self decodePayloadFrom: arg1.!
!
decodePayloadFrom: arg1
	^ self.!
!
encodePayloadOn: arg1
	^ self.!
!

!MQTTPacketVariableHeadedWithPayload methodsFor: 'accessing'!
messageID
	^ msgID.!
!
messageID: arg1
	msgID := arg1.!
!

!MQTTPacketConnect methodsFor: 'initialization'!
initialize
	super initialize.
	willRetainFlag := false.
	cleanSessionFlag := true.
	willQos := 0.
	keepAliveTime := 60.
	protocolVersion := MQTTProtocolLevel50.
	willProperties := MQTT5Properties new.!
!

!MQTTPacketConnect methodsFor: 'accessing'!
authenticationData
	^ authenticationData.!
!
authenticationData: arg1
	authenticationData := arg1.!
!
authenticationMethod
	^ authenticationMethod.!
!
authenticationMethod: arg1
	authenticationMethod := arg1.!
!
cleanSessionFlag: arg1
	cleanSessionFlag := arg1.!
!
clientID: arg1
	clientID := arg1.!
!
clientIDString
	^ clientID ifNil: [ 'CuisMQTT5' ].!
!
keepAliveTime
	^ keepAliveTime.!
!
keepAliveTime: arg1
	keepAliveTime := arg1.!
!
maximumPacketSize
	^ maximumPacketSize.!
!
maximumPacketSize: arg1
	maximumPacketSize := arg1.!
!
packetType
	^ 1.!
!
password
	^ password.!
!
password: arg1
	password := arg1.!
!
protocolLevel: arg1
	protocolVersion := arg1.!
!
protocolVersion
	^ protocolVersion.!
!
protocolVersion: arg1
	protocolVersion := arg1.!
!
receiveMaximum
	^ receiveMaximum.!
!
receiveMaximum: arg1
	receiveMaximum := arg1.!
!
requestProblemInfo
	^ requestProblemInfo.!
!
requestProblemInfo: arg1
	requestProblemInfo := arg1.!
!
requestResponseInfo
	^ requestResponseInfo.!
!
requestResponseInfo: arg1
	requestResponseInfo := arg1.!
!
sessionExpiryInterval
	^ sessionExpiryInterval.!
!
sessionExpiryInterval: arg1
	sessionExpiryInterval := arg1.!
!
topicAliasMaximum
	^ topicAliasMaximum.!
!
topicAliasMaximum: arg1
	topicAliasMaximum := arg1.!
!
user
	^ user.!
!
user: arg1
	user := arg1.!
!
user: arg1 password: arg2
	user := arg1.
	password := arg2.!
!
willProperties
	^ willProperties ifNil: [ willProperties := MQTT5Properties new ].!
!
willProperties: arg1
	willProperties := arg1.!
!
willTopic: arg1 message: arg2 retain: arg3 qos: arg4
	willTopic := arg1.
	willMessage := arg2.
	willRetainFlag := arg3.
	willQos := arg4
		min: 2
		max: 0.!
!

!MQTTPacketConnect methodsFor: 'private'!
connectFlags
	| temp1 |
	temp1 := willQos << 3.
	temp1 := temp1
		bitAt: 2
		put: cleanSessionFlag asBit.
	temp1 := temp1
		bitAt: 3
		put: willTopic notNil asBit.
	temp1 := temp1
		bitAt: 6
		put: willRetainFlag asBit.
	temp1 := temp1
		bitAt: 7
		put: password notNil asBit.
	temp1 := temp1
		bitAt: 8
		put: user notNil asBit.
	^ temp1.!
!

!MQTTPacketConnect methodsFor: 'encoding'!
decodePayloadFrom: arg1
	| temp2 temp3 temp4 temp5 temp6 |
	temp5 := self decodeStringFrom: arg1.
	temp5 = 'MQTT' ifFalse: [
		self debugLog: 'Protocol found: ' , temp5.
		(self connectionRefusedError: 1) signal ].
	temp6 := arg1 next.
	protocolVersion := temp6.
	(temp6 = 4 or: [ temp6 = 5 ]) ifFalse: [ self debugLog: 'version found: ' , temp6 ].
	temp2 := arg1 next.
	(temp2 allMask: 1) ifTrue: [ (self connectionRefusedError: 1) signal ].
	cleanSessionFlag := temp2 allMask: 2.
	willFlag := temp2 allMask: 4.
	willRetainFlag := temp2 allMask: 1 << 5.
	willQos := temp2 >> 3 bitAnd: 3.
	temp3 := temp2 allMask: 1 << 6.
	temp4 := temp2 allMask: 1 << 7.
	keepAliveTime := self decode16BitIntegerFrom: arg1.
	temp6 = 5 ifTrue: [ self decodePropertiesFrom: arg1 ].
	clientID := self decodeStringFrom: arg1.
	clientID size = 0 ifTrue: [ (self connectionRefusedError: 2) signal ].
	willFlag ifTrue: [
		temp6 = 5 ifTrue: [
			willProperties := MQTT5Properties new.
			willProperties decodeFrom: arg1 ].
		willTopic := self decodeStringFrom: arg1.
		willMessage := self decodeByteArrayFrom: arg1 ].
	temp4 ifTrue: [ user := self decodeStringFrom: arg1 ].
	temp3 ifTrue: [ password := self decodeByteArrayFrom: arg1 ].
	returnCode := 0.!
!
decodeVariableHeaderFrom: arg1
	^ self.!
!
encodeConnectPropertiesOn: arg1
	| temp2 |
	temp2 := MQTT5Properties new.
	sessionExpiryInterval ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants sessionExpiryInterval
			value: sessionExpiryInterval ].
	receiveMaximum ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants receiveMaximum
			value: receiveMaximum ].
	maximumPacketSize ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants maximumPacketSize
			value: maximumPacketSize ].
	topicAliasMaximum ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants topicAliasMaximum
			value: topicAliasMaximum ].
	requestResponseInfo ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants requestResponseInformation
			value: requestResponseInfo asBit ].
	requestProblemInfo ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants requestProblemInformation
			value: requestProblemInfo asBit ].
	authenticationMethod ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants authenticationMethod
			value: authenticationMethod ].
	authenticationData ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants authenticationData
			value: authenticationData ].
	properties ifNotNil: [
		properties do: [ :argm9_3 |
			argm9_3 identifier = MQTT5Constants userProperty ifTrue: [ temp2 add: argm9_3 ]]].
	temp2 encodeOn: arg1.!
!
encodePasswordOn: arg1
	password ifNotNil: [
		| data |
		data := password isString
			ifTrue: [ UTF8Encoder encode: password asString ]
			ifFalse: [ password ].
		self
			encodeBinaryData: data
			on: arg1 ].!
!
encodePayloadOn: arg1
	self
		encodeString: self clientIDString
		on: arg1.
	self encodeWillDataOn: arg1.
	self encodeUsernameOn: arg1.
	self encodePasswordOn: arg1.!
!
encodeUsernameOn: arg1
	user ifNotNil: [
		self
			encodeString: user
			on: arg1 ].!
!
encodeVariableHeaderOn: arg1
	self
		encodeString: 'MQTT'
		on: arg1.
	arg1
		nextPut: protocolVersion;
		nextPut: self connectFlags.
	self
		encode16BitInteger: keepAliveTime
		on: arg1.
	protocolVersion = 5 ifTrue: [ self encodeConnectPropertiesOn: arg1 ].!
!
encodeWillDataOn: arg1
	willTopic ifNotNil: [
		protocolVersion = 5 ifTrue: [ willProperties encodeOn: arg1 ].
		self
			encodeString: willTopic
			on: arg1.
		| data |
		data := willMessage
			ifNil: [ ByteArray new ]
			ifNotNil: [
				willMessage isString
					ifTrue: [ UTF8Encoder encode: willMessage asString ]
					ifFalse: [ willMessage ] ].
		self
			encodeBinaryData: data
			on: arg1 ].!
!

!MQTTPacketConnect methodsFor: 'mqtt'!
evaluateFor: arg1
	^ arg1 handleConnectPacket: self.!
!

!MQTTPacketPublish methodsFor: 'initialization'!
initialize
	super initialize.
	qos := 0.
	duplicate := retain := false.
	subscriptionIdentifiers := OrderedCollection new.!
!

!MQTTPacketPublish methodsFor: 'mqtt'!
acknowledgement
	| temp1 |
	qos = 0 ifTrue: [ ^ self badQosError ].
	qos = 1 ifTrue: [ ^ MQTTPacketPubAck new packetID: msgID ].
	temp1 := MQTTPacketPubRec new.
	temp1 packetID: msgID.
	temp1 originalPacket: self.
	^ temp1.!
!
evaluateFor: arg1
	arg1
		handlePublishPacket: self
		qos: qos.!
!
matchesSubscription: arg1 ifTrue: arg2
	(arg1 topicMatches: topic) ifTrue: [
		arg2
			value: topic
			value: message
			value: qos ].!
!
pendingJob
	qos = 0 ifTrue: [ ^ self badQosError ].
	qos = 1 ifTrue: [ ^ MQTTPendingPubAckJob for: self ].
	^ MQTTPendingPubRecJob for: self.!
!
prepareForResend
	duplicate := true.!
!

!MQTTPacketPublish methodsFor: 'encoding'!
decodeFixedHeaderFrom: arg1
	| temp2 |
	(temp2 := arg1 next) >> 4 = self packetType ifFalse: [ self badFixedHeaderError ].
	duplicate := temp2 allMask: 8.
	retain := temp2 allMask: 1.
	qos := temp2 >> 1 bitAnd: 3.
	self decodeLengthFrom: arg1.!
!
decodeFrom: arg1
	| temp2 temp3 temp4 temp5 temp6 |
	self decodeFixedHeaderFrom: arg1.
	temp2 := self decodeVariableHeaderFrom: arg1.
	remainingLength - temp2 > 0
		ifTrue: [
			temp4 := self decodeVariableByteIntegerFrom: arg1.
			temp4 > 0 ifTrue: [
				temp5 := arg1 next: temp4.
				temp6 := ReadStream on: temp5.
				properties := MQTT5Properties new.
				[ temp6 atEnd ] whileFalse: [ properties decodePropertyFrom: temp6 ].
				(properties propertyAt: MQTT5Constants payloadFormatIndicator) ifNotNil: [ :temp7 |
					payloadFormatIndicator := temp7 ].
				(properties propertyAt: MQTT5Constants messageExpiryInterval) ifNotNil: [ :temp8 |
					messageExpiryInterval := temp8 ].
				(properties propertyAt: MQTT5Constants topicAlias) ifNotNil: [ :temp9 |
					topicAlias := temp9 ].
				(properties propertyAt: MQTT5Constants responseTopic) ifNotNil: [ :temp10 |
					responseTopic := temp10 ].
				(properties propertyAt: MQTT5Constants correlationData) ifNotNil: [ :temp11 |
					correlationData := temp11 ].
				(properties propertyAt: MQTT5Constants contentType) ifNotNil: [ :temp12 |
					contentType := temp12 ].
				subscriptionIdentifiers := properties propertiesAt: MQTT5Constants subscriptionIdentifier ].
			message := arg1 next: remainingLength - temp2 - (self variableByteIntegerSizeFor: temp4) - temp4 ]
		ifFalse: [ message := ByteArray new ].!
!
decodePublishPropertiesFrom: arg1
	self decodePropertiesFrom: arg1.
	(properties propertyAt: MQTT5Constants payloadFormatIndicator) ifNotNil: [ :temp2 |
		payloadFormatIndicator := temp2 ].
	(properties propertyAt: MQTT5Constants messageExpiryInterval) ifNotNil: [ :temp3 |
		messageExpiryInterval := temp3 ].
	(properties propertyAt: MQTT5Constants topicAlias) ifNotNil: [ :temp4 |
		topicAlias := temp4 ].
	(properties propertyAt: MQTT5Constants responseTopic) ifNotNil: [ :temp5 |
		responseTopic := temp5 ].
	(properties propertyAt: MQTT5Constants correlationData) ifNotNil: [ :temp6 |
		correlationData := temp6 ].
	(properties propertyAt: MQTT5Constants contentType) ifNotNil: [ :temp7 |
		contentType := temp7 ].
	subscriptionIdentifiers := properties propertiesAt: MQTT5Constants subscriptionIdentifier.!
!
decodeTopicStringFrom: arg1
	| temp2 temp3 |
	temp2 := self decode16BitIntegerFrom: arg1.
	temp3 := arg1 next: temp2.
	topic := [ UTF8Encoder decode: temp3 ]
		on: Error
		do: [ :argm2_4 |
			'Failed Topic Decode' ].
	^ temp2 + 2.!
!
decodeVariableHeaderFrom: arg1
	| temp2 |
	temp2 := self decodeTopicStringFrom: arg1.
	qos > 0 ifTrue: [
		msgID := self decode16BitIntegerFrom: arg1.
		temp2 := temp2 + 2 ].
	^ temp2.!
!
encodePayloadOn: arg1
	arg1 nextPutAll: message.!
!
encodePublishPropertiesOn: arg1
	| temp2 |
	temp2 := MQTT5Properties new.
	payloadFormatIndicator ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants payloadFormatIndicator
			value: payloadFormatIndicator ].
	messageExpiryInterval ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants messageExpiryInterval
			value: messageExpiryInterval ].
	topicAlias ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants topicAlias
			value: topicAlias ].
	responseTopic ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants responseTopic
			value: responseTopic ].
	correlationData ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants correlationData
			value: correlationData ].
	contentType ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants contentType
			value: contentType ].
	subscriptionIdentifiers do: [ :argm7_3 |
		temp2
			addPropertyIdentifier: MQTT5Constants subscriptionIdentifier
			value: argm7_3 ].
	properties do: [ :argm8_3 |
		argm8_3 identifier = MQTT5Constants userProperty ifTrue: [ temp2 add: argm8_3 ]].
	temp2 encodeOn: arg1.!
!
encodeVariableHeaderOn: arg1
	(topicAlias notNil and: [
		topic isNil or: [ topic isEmpty ]])
		ifTrue: [
			self
				encode16BitInteger: 0
				on: arg1 ]
		ifFalse: [
			topic ifNil: [ ^ self badTopicError ].
			self
				encodeString: topic
				on: arg1 ].
	qos > 0 ifTrue: [
		self
			encode16BitInteger: msgID
			on: arg1 ].
	self encodePublishPropertiesOn: arg1.!
!

!MQTTPacketPublish methodsFor: 'accessing'!
addSubscriptionIdentifier: arg1
	subscriptionIdentifiers add: arg1.!
!
contentType
	^ contentType.!
!
contentType: arg1
	contentType := arg1.!
!
correlationData
	^ correlationData.!
!
correlationData: arg1
	correlationData := arg1.!
!
duplicateFlag: arg1
	duplicate := arg1.!
!
fixedHeaderFlags
	^ ((duplicate
		ifTrue: [ 8 ]
		ifFalse: [ 0 ]) bitOr: qos << 1) bitOr:
		(retain
			ifTrue: [ 1 ]
			ifFalse: [ 0 ]).!
!
guessEncodedLength
	^ self message size + 100.!
!
message
	^ message.!
!
message: arg1
	message := arg1.!
!
messageExpiryInterval
	^ messageExpiryInterval.!
!
messageExpiryInterval: arg1
	messageExpiryInterval := arg1.!
!
messageString
	^ UTF8Encoder decode: message.!
!
packetType
	^ 3.!
!
payloadFormatIndicator
	^ payloadFormatIndicator.!
!
payloadFormatIndicator: arg1
	payloadFormatIndicator := arg1.!
!
qos
	^ qos.!
!
qos: arg1
	qos := arg1.!
!
responseTopic
	^ responseTopic.!
!
responseTopic: arg1
	responseTopic := arg1.!
!
retain: arg1
	retain := arg1.!
!
retainFlag: arg1
	retain := arg1.!
!
subscriptionIdentifiers
	^ subscriptionIdentifiers.!
!
topic
	^ topic.!
!
topic: arg1
	((UTF8Encoder encode: arg1 asString) size > 65535 or: [ arg1 includesAnyOf: #($# $+) ]) ifTrue: [ ^ self badTopicError ].
	topic := arg1.!
!
topic: arg1 message: arg2
	((UTF8Encoder encode: arg1 asString) size > 65535 or: [ arg1 includesAnyOf: #($# $+) ]) ifTrue: [ ^ self badTopicError ].
	topic := arg1.
	message := arg2.!
!
topicAlias
	^ topicAlias.!
!
topicAlias: arg1
	topicAlias := arg1.!
!

!MQTTPacketPublish methodsFor: 'as yet unclassified'!
variableByteIntegerSizeFor: arg1
	arg1 < 128 ifTrue: [ ^ 1 ].
	arg1 < 16384 ifTrue: [ ^ 2 ].
	arg1 < 2097152 ifTrue: [ ^ 3 ].
	^ 4.!
!

!MQTTPacketSubAck methodsFor: 'initialization'!
initialize
	super initialize.
	reasonCodes := OrderedCollection new.!
!

!MQTTPacketSubAck methodsFor: 'accessing'!
packetType
	^ 9.!
!
reasonCodes
	^ reasonCodes.!
!
reasonCodes: arg1
	reasonCodes := arg1.!
!
returnCodes
	^ reasonCodes.!
!
returnCodes: arg1
	reasonCodes := arg1.!
!

!MQTTPacketSubAck methodsFor: 'mqtt'!
evaluateFor: arg1
	arg1 handleSubAckPacket: self.!
!

!MQTTPacketSubAck methodsFor: 'testing'!
hasFailures
	^ reasonCodes anySatisfy: [ :argm0_1 |
		argm0_1 >= 128 ].!
!
isSuccess
	^ reasonCodes allSatisfy: [ :argm0_1 |
		argm0_1 <= 2 ].!
!

!MQTTPacketSubAck methodsFor: 'encoding'!
decodePayloadFrom: arg1 length: arg2
	reasonCodes := OrderedCollection new.
	arg2 timesRepeat: [ reasonCodes add: arg1 next ].!
!
decodePropertiesFrom: arg1
	| temp2 temp3 temp4 temp5 |
	temp2 := self decodeVariableByteIntegerFrom: arg1.
	temp2 = 0 ifTrue: [ ^ self ].
	temp3 := arg1 next: temp2.
	temp4 := ReadStream on: temp3.
	properties := MQTT5Properties new.
	[ temp4 atEnd ] whileFalse: [
		temp5 := temp4 next.
		temp5 = 31 ifTrue: [
			properties addProperty:
				(MQTT5Property
					identifier: temp5
					value: (self decodeStringFromReadStream: temp4)) ].
		temp5 = 38 ifTrue: [
			properties addProperty:
				(MQTT5Property
					identifier: temp5
					value:
						(Array
							with: (self decodeStringFromReadStream: temp4)
							with: (self decodeStringFromReadStream: temp4))) ]].!
!
decodeVariableHeaderFrom: arg1
	| temp2 temp3 temp4 temp5 |
	msgID := self decode16BitIntegerFrom: arg1.
	temp2 := self decodeVariableByteIntegerFrom: arg1.
	temp2 > 0 ifTrue: [
		temp3 := arg1 next: temp2.
		temp4 := ReadStream on: temp3.
		properties := MQTT5Properties new.
		[ temp4 atEnd ] whileFalse: [
			temp5 := temp4 next.
			temp5 = 31 ifTrue: [
				properties addProperty:
					(MQTT5Property
						identifier: temp5
						value: (self decodeStringFromReadStream: temp4)) ].
			temp5 = 38 ifTrue: [
				properties addProperty:
					(MQTT5Property
						identifier: temp5
						value:
							(Array
								with: (self decodeStringFromReadStream: temp4)
								with: (self decodeStringFromReadStream: temp4))) ]]].!
!
encodePayloadOn: arg1
	reasonCodes do: [ :argm0_2 |
		arg1 nextPut: argm0_2 ].!
!
encodePropertiesOn: arg1
	(properties isNil or: [ properties isEmpty ]) ifTrue: [
		arg1 nextPut: 0.
		^ self ].
	properties encodeOn: arg1.!
!
encodeVariableHeaderOn: arg1
	self
		encode16BitInteger: msgID
		on: arg1.
	self encodePropertiesOn: arg1.!
!

!MQTTPacketSubAck methodsFor: 'as yet unclassified'!
decodeFrom: arg1
	| temp2 temp3 temp4 temp5 temp6 |
	super decodeFixedHeaderFrom: arg1.
	msgID := self decode16BitIntegerFrom: arg1.
	temp2 := self decodeVariableByteIntegerFrom: arg1.
	temp2 > 0 ifTrue: [
		temp4 := arg1 next: temp2.
		temp5 := ReadStream on: temp4.
		properties := MQTT5Properties new.
		[ temp5 atEnd ] whileFalse: [
			temp6 := temp5 next.
			temp6 = 31 ifTrue: [
				properties addProperty:
					(MQTT5Property
						identifier: temp6
						value: (self decodeStringFromReadStream: temp5)) ].
			temp6 = 38 ifTrue: [
				properties addProperty:
					(MQTT5Property
						identifier: temp6
						value:
							(Array
								with: (self decodeStringFromReadStream: temp5)
								with: (self decodeStringFromReadStream: temp5))) ]]].
	temp3 := remainingLength - 2 - (self variableByteIntegerSizeFor: temp2) - temp2.
	self
		decodePayloadFrom: arg1
		length: temp3.!
!
decodeStringFromReadStream: arg1
	| temp2 temp3 |
	temp2 := (arg1 next bitShift: 8) + arg1 next.
	temp3 := arg1 next: temp2.
	^ UTF8Encoder decode: temp3.!
!
variableByteIntegerSizeFor: arg1
	arg1 < 128 ifTrue: [ ^ 1 ].
	arg1 < 16384 ifTrue: [ ^ 2 ].
	arg1 < 2097152 ifTrue: [ ^ 3 ].
	^ 4.!
!

!MQTTPacketSubscribe methodsFor: 'initialization'!
initialize
	super initialize.
	payloadDict := Dictionary new.!
!

!MQTTPacketSubscribe methodsFor: 'accessing'!
fixedHeaderFlags
	^ 2.!
!
packetType
	^ 8.!
!
subscriptionIdentifier
	^ subscriptionIdentifier.!
!
subscriptionIdentifier: arg1
	subscriptionIdentifier := arg1.!
!
topics
	^ payloadDict keys.!
!

!MQTTPacketSubscribe methodsFor: 'mqtt'!
evaluateFor: arg1
	arg1 handleSubscribePacket: self.!
!
pendingJob
	^ MQTTPendingSubAckJob for: self.!
!

!MQTTPacketSubscribe methodsFor: 'subscription'!
addSubscription: arg1
	payloadDict
		at: arg1 topic
		put: arg1.!
!
addTopic: arg1
	self
		addTopic: arg1
		qos: 0.!
!
addTopic: arg1 qos: arg2
	| temp3 |
	temp3 := MQTTSubscription new
		topic: arg1;
		qos: arg2;
		yourself.
	payloadDict
		at: arg1
		put: temp3.!
!
addTopic: arg1 qos: arg2 noLocal: arg3 retainAsPublished: arg4 retainHandling: arg5
	| temp6 |
	temp6 := MQTTSubscription new
		topic: arg1;
		qos: arg2;
		noLocal: arg3;
		retainAsPublished: arg4;
		retainHandling: arg5;
		yourself.
	payloadDict
		at: arg1
		put: temp6.!
!

!MQTTPacketSubscribe methodsFor: 'encoding'!
decodePayloadFrom: arg1 length: arg2
	| temp3 temp4 temp5 temp6 |
	temp3 := arg1 position + arg2.
	[ arg1 position < temp3 ] whileTrue: [
		temp4 := self decodeStringFrom: arg1.
		temp5 := arg1 next.
		temp6 := MQTTSubscription new
			topic: temp4;
			qos: (temp5 bitAnd: 3);
			noLocal: (temp5 bitAt: 3) = 1;
			retainAsPublished: (temp5 bitAt: 4) = 1;
			retainHandling: ((temp5 bitShift: -4) bitAnd: 3);
			yourself.
		payloadDict
			at: temp4
			put: temp6 ].!
!
decodePropertiesFrom: arg1
	| temp2 temp3 temp4 |
	temp2 := self decodeVariableByteIntegerFrom: arg1.
	temp2 = 0 ifTrue: [ ^ self ].
	temp3 := arg1 position + temp2.
	[ arg1 position < temp3 ] whileTrue: [
		temp4 := arg1 next.
		temp4 = 11
			ifTrue: [ subscriptionIdentifier := self decodeVariableByteIntegerFrom: arg1 ]
			ifFalse: [
				temp4 = 38 ifTrue: [
					properties ifNil: [ properties := MQTT5Properties new ].
					properties addProperty:
						(MQTT5Property
							identifier: temp4
							value:
								(Array
									with: (self decodeStringFrom: arg1)
									with: (self decodeStringFrom: arg1))) ]]].!
!
decodeVariableHeaderFrom: arg1
	msgID := self decode16BitIntegerFrom: arg1.
	self decodePropertiesFrom: arg1.!
!
encodePayloadOn: arg1
	payloadDict keysAndValuesDo: [ :argm0_2 :argm0_3 |
		self
			encodeString: argm0_2
			on: arg1.
		arg1 nextPut: argm0_3 subscriptionOptionsV5 ].!
!
encodePropertiesOn: arg1
	| temp2 |
	(subscriptionIdentifier isNil and: [
		properties isNil or: [ properties isEmpty ]]) ifTrue: [
		arg1 nextPut: 0.
		^ self ].
	temp2 := MQTTWriteStream on: (ByteArray new: 32).
	subscriptionIdentifier ifNotNil: [
		temp2 nextPut: 11.
		self
			encodeVariableByteInteger: subscriptionIdentifier
			on: temp2 ].
	properties ifNotNil: [
		properties do: [ :argm3_3 |
			argm3_3 encodeOn: temp2 ]].
	self
		encodeVariableByteInteger: temp2 position
		on: arg1.
	arg1 nextPutAll: temp2 contents.!
!
encodeVariableHeaderOn: arg1
	self
		encode16BitInteger: msgID
		on: arg1.
	self encodePropertiesOn: arg1.!
!

!MQTTPacketUnsubscribe methodsFor: 'initialization'!
initialize
	super initialize.
	topics := OrderedCollection new.!
!

!MQTTPacketUnsubscribe methodsFor: 'accessing'!
fixedHeaderFlags
	^ 2.!
!
packetType
	^ 10.!
!
topics
	^ topics.!
!
topics: arg1
	topics := arg1.!
!

!MQTTPacketUnsubscribe methodsFor: 'subscription'!
addTopic: arg1
	topics add: arg1.!
!

!MQTTPacketUnsubscribe methodsFor: 'mqtt'!
evaluateFor: arg1
	arg1 handleUnsubscribePacket: self.!
!
pendingJob
	^ MQTTPendingUnsubAckJob for: self.!
!

!MQTTPacketUnsubscribe methodsFor: 'encoding'!
decodePayloadFrom: arg1 length: arg2
	| temp3 |
	topics := OrderedCollection new.
	temp3 := arg1 position + arg2.
	[ arg1 position < temp3 ] whileTrue: [ topics add: (self decodeStringFrom: arg1) ].!
!
decodePropertiesFrom: arg1
	| temp2 temp3 temp4 |
	temp2 := self decodeVariableByteIntegerFrom: arg1.
	temp2 = 0 ifTrue: [ ^ self ].
	temp3 := arg1 position + temp2.
	properties := MQTT5Properties new.
	[ arg1 position < temp3 ] whileTrue: [
		temp4 := arg1 next.
		temp4 = 38 ifTrue: [
			properties addProperty:
				(MQTT5Property
					identifier: temp4
					value:
						(Array
							with: (self decodeStringFrom: arg1)
							with: (self decodeStringFrom: arg1))) ]].!
!
decodeVariableHeaderFrom: arg1
	msgID := self decode16BitIntegerFrom: arg1.
	self decodePropertiesFrom: arg1.!
!
encodePayloadOn: arg1
	topics do: [ :argm0_2 |
		self
			encodeString: argm0_2
			on: arg1 ].!
!
encodePropertiesOn: arg1
	(properties isNil or: [ properties isEmpty ]) ifTrue: [
		arg1 nextPut: 0.
		^ self ].
	properties encodeOn: arg1.!
!
encodeVariableHeaderOn: arg1
	self
		encode16BitInteger: msgID
		on: arg1.
	self encodePropertiesOn: arg1.!
!
Object subclass: #MQTTTransportLayer
	instanceVariableNames: 'sockStrm abort incomingPacketProcess packetInFlightQueue outgoingPacketQueue outgoingPacketProcess retryTime pendingJobsMutex abortMutex retryProcess lastMID pendingJobs currentSubscriptions readWaitTime statisticsMutex statistics protocolVersion'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTTransportLayer subclass: #MQTTTransportLayerClient
	instanceVariableNames: 'pingOK keepAliveTime clientID socketClient serverCapabilities topicAliasMapOutgoing topicAliasMapIncoming'
	classVariableNames: 'ClientID'
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

Object subclass: #MQTTSocketClient
	instanceVariableNames: 'transport cleanSessionFlag willTopic willMessage willRetainFlag willQos userNameString passwordString keepAliveTime socket interface pendingSocket pendingClientID protocolVersion'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

Object subclass: #MQTTClientInterface
	instanceVariableNames: 'socketClient hostName keepAliveTime protocolVersion'
	classVariableNames: 'TranscriptMutex'
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

Object subclass: #MQTTSubscription
	instanceVariableNames: 'topic actionBlock qos retainFlag noLocal retainAsPublished retainHandling'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

Object subclass: #MQTTPacketAndPendingJobPair
	instanceVariableNames: 'packet pendingJob'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

Object subclass: #MQTTPendingJob
	instanceVariableNames: 'originalPacket sendTime'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPendingJob subclass: #MQTTPendingPingJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPendingJob subclass: #MQTTPendingPubAckJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPendingJob subclass: #MQTTPendingPubCompJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPendingJob subclass: #MQTTPendingPubRecJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPendingJob subclass: #MQTTPendingPubRelJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPendingJob subclass: #MQTTPendingSubAckJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPendingJob subclass: #MQTTPendingUnsubAckJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!


!MQTTTransportLayer methodsFor: 'initialization'!
initialize
	super initialize.
	abort := false.
	retryTime := 30.
	readWaitTime := 0.
	lastMID := 0.
	packetInFlightQueue := SharedQueue new.
	outgoingPacketQueue := SharedQueue new.
	pendingJobs := Dictionary new.
	currentSubscriptions := Dictionary new.
	pendingJobsMutex := Semaphore forMutualExclusion.
	abortMutex := Semaphore forMutualExclusion.
	statisticsMutex := Semaphore forMutualExclusion.
	statistics := MQTTStatistics new.
	protocolVersion := 5.! !

!MQTTTransportLayer methodsFor: 'accessing'!
abort
	^ abort.! !

!MQTTTransportLayer methodsFor: 'accessing'!
abort: arg1
	abortMutex critical: [ abort := arg1 ].! !

!MQTTTransportLayer methodsFor: 'accessing'!
isVersion5
	^ protocolVersion = 5.! !

!MQTTTransportLayer methodsFor: 'accessing'!
outgoingPacketQueue
	^ outgoingPacketQueue.! !

!MQTTTransportLayer methodsFor: 'accessing'!
outgoingPacketQueue: arg1
	outgoingPacketQueue := arg1.! !

!MQTTTransportLayer methodsFor: 'accessing'!
packetInFlightQueue
	^ packetInFlightQueue.! !

!MQTTTransportLayer methodsFor: 'accessing'!
packetInFlightQueue: arg1
	packetInFlightQueue := arg1.! !

!MQTTTransportLayer methodsFor: 'accessing'!
protocolVersion
	^ protocolVersion.! !

!MQTTTransportLayer methodsFor: 'accessing'!
protocolVersion: arg1
	protocolVersion := arg1.! !

!MQTTTransportLayer methodsFor: 'accessing'!
readWaitTime
	^ readWaitTime.! !

!MQTTTransportLayer methodsFor: 'accessing'!
readWaitTime: arg1
	readWaitTime := arg1.! !

!MQTTTransportLayer methodsFor: 'accessing'!
retryTime
	^ retryTime.! !

!MQTTTransportLayer methodsFor: 'accessing'!
retryTime: arg1
	retryTime := arg1.! !

!MQTTTransportLayer methodsFor: 'accessing'!
sockStrm
	^ sockStrm.! !

!MQTTTransportLayer methodsFor: 'accessing'!
sockStrm: arg1
	sockStrm := arg1.! !

!MQTTTransportLayer methodsFor: 'accessing'!
statistics
	^ statistics.! !

!MQTTTransportLayer methodsFor: 'message ids'!
nextMsgID
	lastMID := lastMID \\ 65535 + 1.
	^ lastMID.! !

!MQTTTransportLayer methodsFor: 'subscriptions'!
addSubscription: arg1
	currentSubscriptions
		at: arg1 topic
		put: arg1.! !

!MQTTTransportLayer methodsFor: 'subscriptions'!
currentSubscriptions
	^ currentSubscriptions.! !

!MQTTTransportLayer methodsFor: 'subscriptions'!
removeSubscription: arg1
	currentSubscriptions
		removeKey: arg1
		ifAbsent: [ ].! !

!MQTTTransportLayer methodsFor: 'pending jobs'!
addPendingJob: arg1 forPacketID: arg2
	pendingJobsMutex critical: [
		pendingJobs
			at: arg2
			put: arg1 ].! !

!MQTTTransportLayer methodsFor: 'pending jobs'!
pendingJobForPacketID: arg1
	^ pendingJobsMutex critical: [
		pendingJobs
			at: arg1
			ifAbsent: [ ]].! !

!MQTTTransportLayer methodsFor: 'pending jobs'!
removePendingJobForPacketID: arg1
	pendingJobsMutex critical: [
		pendingJobs
			removeKey: arg1
			ifAbsent: [ ]].! !

!MQTTTransportLayer methodsFor: 'mqtt'!
badTopicError
	MQTTBadTopicException signal.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
debugLog: arg1
	MQTTClientInterface
		debugLog: self printString
		tag: ' TL '
		str2: arg1.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
disconnect: arg1
	(self preambleWorkAbortEarlyIfTrue: arg1) ifTrue: [ ^ self ].! !

!MQTTTransportLayer methodsFor: 'mqtt'!
disconnectPostCleanup
	sockStrm ifNotNil: [
		[ sockStrm close ]
			on: Error
			do: [ :argm0_1 |
				nil ].
		sockStrm := nil ].! !

!MQTTTransportLayer methodsFor: 'mqtt'!
doSubscriptionActionsFor: arg1
	currentSubscriptions valuesDo: [ :argm0_2 |
		arg1
			matchesSubscription: argm0_2
			ifTrue: argm0_2 actionBlock ].! !

!MQTTTransportLayer methodsFor: 'mqtt'!
exceptionHandler: arg1
	self debugLog: 'Exception: ' , arg1 printString.
	self
		statPerform: #errorTypes:
		with: arg1 class name.
	arg1 class = MQTTEmptyStreamErrorException ifTrue: [ (Delay forMilliseconds: 100) wait ].
	^ nil.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
findAndRunJobsOverdueForAcknowledgement
	| temp1 |
	temp1 := OrderedCollection new.
	pendingJobsMutex critical: [
		pendingJobs keysAndValuesDo: [ :argm1_2 :argm1_3 |
			argm1_3 isOverdue ifTrue: [ temp1 add: argm1_3 ]]].
	temp1 do: [ :argm2_2 |
		self debugLog: 'Resending packet for job: ' , argm2_2 printString.
		argm2_2 resendOn: self ].! !

!MQTTTransportLayer methodsFor: 'mqtt'!
handleConnAckPacket: arg1
	^ self.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
handleDisconnect: arg1
	self debugLog: 'Received DISCONNECT from server'.
	self disconnect: false.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
handleIncomingPacket: arg1
	| temp2 |
	temp2 := arg1 packetType.
	temp2 = 2 ifTrue: [ ^ self handleConnAckPacket: arg1 ].
	temp2 = 3 ifTrue: [ ^ self
			handlePublishPacket: arg1
			qos: arg1 qos ].
	temp2 = 4 ifTrue: [ ^ self handlePubAckPacket: arg1 ].
	temp2 = 5 ifTrue: [ ^ self handlePubRecPacket: arg1 ].
	temp2 = 6 ifTrue: [ ^ self handlePubRelPacket: arg1 ].
	temp2 = 7 ifTrue: [ ^ self handlePubCompPacket: arg1 ].
	temp2 = 9 ifTrue: [ ^ self handleSubAckPacket: arg1 ].
	temp2 = 11 ifTrue: [ ^ self handleUnsubAckPacket: arg1 ].
	temp2 = 13 ifTrue: [ ^ self handlePingResp ].
	temp2 = 14 ifTrue: [ ^ self handleDisconnect: arg1 ].
	self debugLog: 'Unknown packet type: ' , temp2 printString.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
handlePingResp
	^ self.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
handlePubAckPacket: arg1
	| temp2 |
	temp2 := self pendingJobForPacketID: arg1 packetID.
	temp2 ifNil: [ ^ self missingPendingJob: #pubAck ].
	self removePendingJobForPacketID: arg1 packetID.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
handlePubCompPacket: arg1
	| temp2 |
	temp2 := self pendingJobForPacketID: arg1 packetID.
	temp2 ifNil: [ ^ self missingPendingJob: #pubComp ].
	self removePendingJobForPacketID: arg1 packetID.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
handlePubRecPacket: arg1
	| temp2 |
	temp2 := self pendingJobForPacketID: arg1 packetID.
	temp2 ifNil: [ ^ self missingPendingJob: #pubRec ].
	self sendPacket: arg1 acknowledgement.
	self removePendingJobForPacketID: arg1 packetID.
	self
		addPendingJob: (MQTTPendingPubCompJob for: arg1)
		forPacketID: arg1 packetID.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
handlePubRelPacket: arg1
	| temp2 |
	temp2 := self pendingJobForPacketID: arg1 packetID.
	temp2 ifNil: [ self missingPendingJob: #pubRel ].
	self sendPacket: arg1 acknowledgement.
	temp2 ifNotNil: [
		self removePendingJobForPacketID: arg1 packetID.
		self doSubscriptionActionsFor: temp2 originalPacket ].! !

!MQTTTransportLayer methodsFor: 'mqtt'!
handlePublishPacket: arg1 qos: arg2
	| temp3 temp4 |
	arg2 >= 1 ifTrue: [
		temp3 := arg1 acknowledgement.
		arg2 = 1 ifTrue: [ self sendPacket: temp3 ]].
	arg2 = 2 ifTrue: [
		(self pendingJobForPacketID: arg1 packetID) ifNotNil: [ ^ self ].
		temp4 := MQTTPendingPubRelJob for: temp3.
		self
			addPendingJob: temp4
			forPacketID: arg1 packetID.
		self sendPacket:
			(MQTTPacketAndPendingJobPair
				packet: temp3
				job: temp4).
		^ self ].
	self doSubscriptionActionsFor: arg1.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
handleSubAckPacket: arg1
	| temp2 |
	temp2 := self pendingJobForPacketID: arg1 packetID.
	temp2 ifNil: [ ^ self missingPendingJob: #subAck ].
	self removePendingJobForPacketID: arg1 packetID.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
handleUnsubAckPacket: arg1
	| temp2 |
	temp2 := self pendingJobForPacketID: arg1 packetID.
	temp2 ifNil: [ ^ self missingPendingJob: #unSubAck ].
	self removePendingJobForPacketID: arg1 packetID.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
initializeKeepAlive
	^ self.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
initializeMQTTConnection: arg1
	^ self.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
initializePacketReading
	| temp1 temp2 |
	temp1 := self packetReader.
	incomingPacketProcess := [
	[
	temp2 := nil.
	temp2 := [ temp1 value ]
		on: Error
		do: [ :argm6_4 |
			self exceptionHandler: argm6_4 ].
	temp2 ifNotNil: [
		self
			statPerform: #packetIn:
			with: temp2.
		(temp2 packetType = 3 and: [ temp2 qos > 0 ]) ifTrue: [ self debugLog: 'Rx ' , temp2 asString ].
		[ self handleIncomingPacket: temp2 ]
			on: Error
			do: [ :argm9_4 |
				self exceptionHandler: argm9_4 ]].
	Processor yield.
	self abort ] whileFalse ] fork.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
initializePacketWriting
	| temp1 temp2 |
	temp1 := self packetWriter.
	outgoingPacketProcess := [
	[
	temp2 := nil.
	temp2 := self packetInFlightQueue nextOrNil.
	temp2 ifNil: [ temp2 := self outgoingPacketQueue next ].
	self packetInFlightQueue nextPut: temp2.
	self outgoingPacketQueue size > 10 ifTrue: [ self debugLog: 'write packet backlog at: ' , self outgoingPacketQueue size printString ].
	[ temp1 value: temp2 ]
		on: Error
		do: [ :argm14_4 |
			self exceptionHandler: argm14_4 ].
	self abort ifFalse: [ self packetInFlightQueue nextOrNil ].
	Processor yield.
	self abort ] whileFalse ] fork.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
initializeRetryProcess
	retryProcess := [ | temp0_1 |
	temp0_1 := Delay forSeconds: 2.
	[
	self abort ifFalse: [ self findAndRunJobsOverdueForAcknowledgement ].
	temp0_1 wait.
	self abort ] whileFalse ] fork.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
initializeSocketStream: arg1
	sockStrm := SocketStream on: arg1.
	sockStrm binary.
	sockStrm timeout: self readWaitTime.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
missingPendingJob: arg1
	self debugLog: '************ No matching pending job for type: ' , arg1 asString.
	self
		statPerform: #missingType:
		with: arg1.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
onTopic: arg1 qos: arg2 do: arg3
	| temp4 |
	temp4 := MQTTSubscription
		for: arg1
		qos: arg2
		do: arg3.
	temp4 ifNil: [ ^ self badTopicError ].
	self addSubscription: temp4.
	self
		subscribe: arg1
		qos: arg2.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
packetReader
	^ [ MQTTPacket readFrom: sockStrm ].! !

!MQTTTransportLayer methodsFor: 'mqtt'!
packetWriter
	^ [ :argm0_1 | | temp0_2 |
	temp0_2 := (argm0_1 isKindOf: MQTTPacketAndPendingJobPair)
		ifTrue: [ argm0_1 packet ]
		ifFalse: [ argm0_1 ].
	self debugLog: 'Wx ' , temp0_2 asString.
	temp0_2 encodeOn: sockStrm.
	self
		statPerform: #packetOut:
		with: temp0_2 ].! !

!MQTTTransportLayer methodsFor: 'mqtt'!
preambleWorkAbortEarlyIfTrue: arg1
	abortMutex critical: [
		abort ifTrue: [ ^ true ].
		self debugLog: 'disconnecting with restart: ' , arg1 printString.
		self statPerform: #disconnect.
		self sendPossibleDisconnectPackage.
		abort := true.
		self disconnectPostCleanup ].
	^ false.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
publishTopic: arg1 message: arg2 qos: arg3 retain: arg4
	| temp5 temp6 |
	temp5 := MQTTPacketPublish new
		topic: arg1
		message: arg2 asByteArray;
		retainFlag: arg4;
		qos: arg3.
	arg3 > 0 ifTrue: [
		temp5 messageID: self nextMsgID.
		temp6 := temp5 pendingJob.
		self
			addPendingJob: temp6
			forPacketID: temp5 messageID ].
	self sendPacket:
		(MQTTPacketAndPendingJobPair
			packet: temp5
			job: temp6).! !

!MQTTTransportLayer methodsFor: 'mqtt'!
restart: arg1
	(Delay forSeconds: 3) wait.
	self debugLog: 'reStart Socket'.
	abortMutex critical: [ abort := false ].
	self initializeSocketStream: arg1.
	self
		initializeMQTTConnection: false;
		initializePacketReading;
		initializePacketWriting;
		initializeRetryProcess.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
sendPacket: arg1
	outgoingPacketQueue nextPut: arg1.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
sendPossibleDisconnectPackage
	^ self.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
start: arg1
	self debugLog: 'Start Socket'.
	abort := false.
	retryTime := 20.
	readWaitTime := 30.
	pendingJobsMutex := Semaphore forMutualExclusion.
	currentSubscriptions := Dictionary new.
	outgoingPacketQueue := SharedQueue new.
	packetInFlightQueue := SharedQueue new.
	statistics := MQTTStatistics new.
	statisticsMutex := Semaphore forMutualExclusion.
	abortMutex := Semaphore forMutualExclusion.
	self initializeSocketStream: arg1.
	self
		initializeMQTTConnection: true;
		initializePacketReading;
		initializePacketWriting;
		initializeRetryProcess.
	self debugLog: 'Started Socket'.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
statPerform: arg1
	statisticsMutex critical: [
		[ statistics perform: arg1 ]
			on: Error
			do: [ :argm0_2 |
				nil ]].! !

!MQTTTransportLayer methodsFor: 'mqtt'!
statPerform: arg1 with: arg2
	statisticsMutex critical: [
		[
		statistics
			perform: arg1
			with: arg2 ]
			on: Error
			do: [ :argm0_3 |
				nil ]].! !

!MQTTTransportLayer methodsFor: 'mqtt'!
subscribe: arg1 qos: arg2
	| temp3 temp4 |
	temp3 := MQTTPacketSubscribe new
		addTopic: arg1
		qos: arg2;
		packetID: self nextMsgID.
	temp4 := temp3 pendingJob.
	self
		addPendingJob: temp4
		forPacketID: temp3 packetID.
	self sendPacket:
		(MQTTPacketAndPendingJobPair
			packet: temp3
			job: temp4).! !

!MQTTTransportLayer methodsFor: 'mqtt'!
transportType
	^ ' TL '.! !

!MQTTTransportLayer methodsFor: 'mqtt'!
unsubscribeFrom: arg1
	| temp2 temp3 |
	self removeSubscription: arg1.
	temp2 := MQTTPacketUnsubscribe new
		addTopic: arg1;
		packetID: self nextMsgID.
	temp3 := temp2 pendingJob.
	self
		addPendingJob: temp3
		forPacketID: temp2 packetID.
	self sendPacket:
		(MQTTPacketAndPendingJobPair
			packet: temp2
			job: temp3).! !

!MQTTTransportLayerClient methodsFor: 'initialization'!
initialize
	super initialize.
	pingOK := false.
	keepAliveTime := 60.
	serverCapabilities := MQTT5ServerCapabilities new.
	topicAliasMapOutgoing := Dictionary new.
	topicAliasMapIncoming := Dictionary new.! !

!MQTTTransportLayerClient methodsFor: 'accessing'!
clientID
	^ clientID.! !

!MQTTTransportLayerClient methodsFor: 'accessing'!
clientID: arg1
	clientID := arg1.! !

!MQTTTransportLayerClient methodsFor: 'accessing'!
clientIDString
	^ clientID ifNil: [ clientID := self class nextClientID ].! !

!MQTTTransportLayerClient methodsFor: 'accessing'!
keepAliveTime
	^ keepAliveTime.! !

!MQTTTransportLayerClient methodsFor: 'accessing'!
keepAliveTime: arg1
	keepAliveTime := arg1.! !

!MQTTTransportLayerClient methodsFor: 'accessing'!
pingOK
	^ pingOK.! !

!MQTTTransportLayerClient methodsFor: 'accessing'!
pingOK: arg1
	pingOK := arg1.! !

!MQTTTransportLayerClient methodsFor: 'accessing'!
socketClient
	^ socketClient.! !

!MQTTTransportLayerClient methodsFor: 'accessing'!
socketClient: arg1
	socketClient := arg1.! !

!MQTTTransportLayerClient methodsFor: 'v5 capabilities'!
serverCapabilities
	^ serverCapabilities.! !

!MQTTTransportLayerClient methodsFor: 'v5 capabilities'!
serverCapabilities: arg1
	serverCapabilities := arg1.! !

!MQTTTransportLayerClient methodsFor: 'v5 capabilities'!
updateServerCapabilities: arg1
	self isVersion5 ifFalse: [ ^ self ].
	arg1 properties ifNotNil: [ :temp2 |
		serverCapabilities updateFromConnAckProperties: temp2 ].
	serverCapabilities assignedClientIdentifier ifNotNil: [ :temp3 |
		clientID := temp3 ].! !

!MQTTTransportLayerClient methodsFor: 'v5 topic alias'!
clearTopicAliasMaps
	topicAliasMapOutgoing := Dictionary new.
	topicAliasMapIncoming := Dictionary new.! !

!MQTTTransportLayerClient methodsFor: 'v5 topic alias'!
nextTopicAliasFor: arg1
	| temp2 temp3 temp4 |
	self isVersion5 ifFalse: [ ^ nil ].
	temp2 := serverCapabilities topicAliasMaximum.
	temp2 = 0 ifTrue: [ ^ nil ].
	temp3 := topicAliasMapOutgoing
		keyAtValue: arg1
		ifAbsent: [ ].
	temp3 ifNotNil: [ ^ temp3 ].
	topicAliasMapOutgoing size < temp2 ifTrue: [
		temp4 := topicAliasMapOutgoing size + 1.
		topicAliasMapOutgoing
			at: temp4
			put: arg1.
		^ temp4 ].
	^ nil.! !

!MQTTTransportLayerClient methodsFor: 'v5 topic alias'!
registerTopicAliasIncoming: arg1 forTopic: arg2
	topicAliasMapIncoming
		at: arg1
		put: arg2.! !

!MQTTTransportLayerClient methodsFor: 'v5 topic alias'!
resolveTopicAliasIncoming: arg1
	^ topicAliasMapIncoming
		at: arg1
		ifAbsent: [ ].! !

!MQTTTransportLayerClient methodsFor: 'v5 topic alias'!
topicAliasMapIncoming
	^ topicAliasMapIncoming.! !

!MQTTTransportLayerClient methodsFor: 'v5 topic alias'!
topicAliasMapOutgoing
	^ topicAliasMapOutgoing.! !

!MQTTTransportLayerClient methodsFor: 'mqtt'!
disconnect: arg1
	(self preambleWorkAbortEarlyIfTrue: arg1) ifTrue: [ ^ self ].
	self clearTopicAliasMaps.
	arg1 ifTrue: [
		self socketClient ifNotNil: [ :temp2 |
			temp2 interface restart ]].! !

!MQTTTransportLayerClient methodsFor: 'mqtt'!
handleConnAckPacket: arg1
	| temp2 |
	temp2 := arg1 returnCode ifNil: [ arg1 reasonCode ifNil: [ 0 ]].
	temp2 = 0 ifFalse: [
		self debugLog: 'CONNACK failed with code: ' , temp2 printString.
		MQTTCONNECTException connectReturnCode: temp2 ].
	self isVersion5 ifTrue: [ self updateServerCapabilities: arg1 ].
	self debugLog: 'CONNACK received - connection established'.
	pingOK := true.
	self initializeKeepAlive.! !

!MQTTTransportLayerClient methodsFor: 'mqtt'!
handlePingResp
	pingOK := true.
	self debugLog: 'PINGRESP received'.! !

!MQTTTransportLayerClient methodsFor: 'mqtt'!
initializeKeepAlive
	keepAliveTime > 0 ifFalse: [ ^ self ].
	[ | temp1_1 |
	temp1_1 := Delay forSeconds: (keepAliveTime // 2 max: 10).
	[
	pingOK ifTrue: [
		pingOK := false.
		self sendPacket: MQTTPacketPingReq new ].
	temp1_1 wait.
	self abort ] whileFalse ] fork.! !

!MQTTTransportLayerClient methodsFor: 'mqtt'!
sendPossibleDisconnectPackage
	[ | temp0_1 |
	temp0_1 := MQTTPacketDisconnect new.
	self isVersion5 ifTrue: [ temp0_1 reasonCode: 0 ].
	temp0_1 encodeOn: sockStrm ]
		on: Error
		do: [ :argm0_1 |
			nil ].! !

!MQTTTransportLayerClient methodsFor: 'mqtt'!
transportType
	^ ' TC '.! !

!MQTTTransportLayerClient class methodsFor: 'class initialization'!
initialize
	ClientID := 0.! !

!MQTTTransportLayerClient class methodsFor: 'accessing'!
nextClientID
	ClientID := ClientID + 1.
	^ 'MQTTClient_' , ClientID printString.! !

!MQTTSocketClient methodsFor: 'initialization'!
initialize
	super initialize.
	cleanSessionFlag := true.
	willQos := 0.
	keepAliveTime := 60.
	protocolVersion := 5.! !

!MQTTSocketClient methodsFor: 'accessing'!
cleanSessionFlag
	^ cleanSessionFlag.! !

!MQTTSocketClient methodsFor: 'accessing'!
cleanSessionFlag: arg1
	cleanSessionFlag := arg1.! !

!MQTTSocketClient methodsFor: 'accessing'!
clientID: arg1
	pendingClientID := arg1.! !

!MQTTSocketClient methodsFor: 'accessing'!
interface
	^ interface.! !

!MQTTSocketClient methodsFor: 'accessing'!
interface: arg1
	interface := arg1.
	transport := interface socketClient ifNotNil: [ interface socketClient transport ].! !

!MQTTSocketClient methodsFor: 'accessing'!
keepAliveTime
	^ keepAliveTime.! !

!MQTTSocketClient methodsFor: 'accessing'!
keepAliveTime: arg1
	keepAliveTime := arg1.! !

!MQTTSocketClient methodsFor: 'accessing'!
passwordString
	^ passwordString.! !

!MQTTSocketClient methodsFor: 'accessing'!
passwordString: arg1
	passwordString := arg1.! !

!MQTTSocketClient methodsFor: 'accessing'!
pendingSocket
	^ pendingSocket.! !

!MQTTSocketClient methodsFor: 'accessing'!
pendingSocket: arg1
	pendingSocket := arg1.! !

!MQTTSocketClient methodsFor: 'accessing'!
protocolVersion
	^ protocolVersion.! !

!MQTTSocketClient methodsFor: 'accessing'!
protocolVersion: arg1
	protocolVersion := arg1.! !

!MQTTSocketClient methodsFor: 'accessing'!
transport
	^ transport.! !

!MQTTSocketClient methodsFor: 'accessing'!
transport: arg1
	transport := arg1.! !

!MQTTSocketClient methodsFor: 'accessing'!
userNameString
	^ userNameString.! !

!MQTTSocketClient methodsFor: 'accessing'!
userNameString: arg1
	userNameString := arg1.! !

!MQTTSocketClient methodsFor: 'accessing'!
willMessage
	^ willMessage.! !

!MQTTSocketClient methodsFor: 'accessing'!
willMessage: arg1
	willMessage := arg1.! !

!MQTTSocketClient methodsFor: 'accessing'!
willQos
	^ willQos.! !

!MQTTSocketClient methodsFor: 'accessing'!
willQos: arg1
	willQos := arg1.! !

!MQTTSocketClient methodsFor: 'accessing'!
willRetainFlag
	^ willRetainFlag.! !

!MQTTSocketClient methodsFor: 'accessing'!
willRetainFlag: arg1
	willRetainFlag := arg1.! !

!MQTTSocketClient methodsFor: 'accessing'!
willTopic
	^ willTopic.! !

!MQTTSocketClient methodsFor: 'accessing'!
willTopic: arg1
	willTopic := arg1.! !

!MQTTSocketClient methodsFor: 'mqtt'!
acceptFrom: arg1
	self debugLog: 'MQTTSocketClient saw connectedOn: on socket'.
	transport isNil
		ifTrue: [
			transport := MQTTTransportLayerClient new.
			transport keepAliveTime: self keepAliveTime.
			transport socketClient: self.
			transport protocolVersion: self protocolVersion.
			pendingClientID ifNotNil: [ transport clientID: pendingClientID ].
			transport start: arg1.
			self initializeMQTTConnection: true ]
		ifFalse: [
			transport restart: arg1.
			self initializeMQTTConnection: false ].! !

!MQTTSocketClient methodsFor: 'mqtt'!
close
	self debugLog: 'MQTTSocketClient saw close on socket'.
	self transport ifNotNil: [ self transport disconnect: true ].
	self release.! !

!MQTTSocketClient methodsFor: 'mqtt'!
connect
	pendingSocket ifNil: [ self error: 'No pending socket - was openOnHostName called?' ].
	self acceptFrom: pendingSocket.
	pendingSocket := nil.! !

!MQTTSocketClient methodsFor: 'mqtt'!
debugLog: arg1
	MQTTClientInterface
		debugLog: self printString
		tag: ' SC '
		str2: arg1.! !

!MQTTSocketClient methodsFor: 'mqtt'!
disconnect
	self transport ifNotNil: [ self transport disconnect: false ].! !

!MQTTSocketClient methodsFor: 'mqtt'!
initializeMQTTConnection: arg1
	| temp2 temp3 |
	temp2 := MQTTPacketConnect new keepAliveTime: self keepAliveTime.
	willTopic isString & willMessage isString & (willRetainFlag isKindOf: Boolean) & willQos isInteger ifTrue: [
		temp2
			willTopic: willTopic
			message: willMessage
			retain: willRetainFlag
			qos: willQos ].
	cleanSessionFlag := arg1.
	temp2 cleanSessionFlag: cleanSessionFlag.
	temp2
		user: userNameString
		password: passwordString.
	temp2 clientID: self transport clientIDString.
	self protocolVersion = 5 ifTrue: [ temp2 protocolLevel: 5 ].
	arg1
		ifTrue: [
			self transport outgoingPacketQueue: SharedQueue new.
			self transport packetInFlightQueue: SharedQueue new.
			self transport sendPacket: temp2 ]
		ifFalse: [
			temp3 := SharedQueue new.
			temp3 nextPut: temp2.
			(self transport packetInFlightQueue contents) do: [ :each | temp3 nextPut: each ].
			self transport packetInFlightQueue: temp3 ].! !

!MQTTSocketClient methodsFor: 'mqtt'!
onTopic: arg1 qos: arg2 do: arg3
	self transport
		onTopic: arg1
		qos: arg2
		do: arg3.! !

!MQTTSocketClient methodsFor: 'mqtt'!
publishTopic: arg1 message: arg2 qos: arg3 retain: arg4
	self transport
		publishTopic: arg1
		message: arg2
		qos: arg3
		retain: arg4.! !

!MQTTSocketClient methodsFor: 'mqtt'!
readWaitTime: arg1
	self transport readWaitTime: arg1.! !

!MQTTSocketClient methodsFor: 'mqtt'!
release
	interface := nil.
	transport := nil.! !

!MQTTSocketClient methodsFor: 'mqtt'!
unsubscribeFrom: arg1
	self transport unsubscribeFrom: arg1.! !

!MQTTSocketClient methodsFor: 'mqtt'!
username: arg1 password: arg2
	arg1 ifNotNil: [ userNameString := UTF8Encoder encode: arg1 asString ].
	arg2 ifNotNil: [ passwordString := UTF8Encoder encode: arg2 asString ].! !

!MQTTSocketClient methodsFor: 'mqtt'!
willTopic: arg1 message: arg2 retain: arg3 qos: arg4
	arg1 isString & arg2 isString & (arg3 = true | arg3 = false) &
		(arg4 isInteger &
			(arg4
				between: 0
				and: 2)) ifFalse: [ ^ self error: 'Bad Will' ].
	willTopic := arg1.
	willMessage := arg2.
	willRetainFlag := arg3.
	willQos := arg4.! !

!MQTTSocketClient class methodsFor: 'instance creation'!
openOnHostName: arg1 port: arg2 keepAlive: arg3 interface: arg4
	| temp5 temp6 temp7 |
	temp7 := NetNameResolver
		addressForName: arg1
		timeout: arg3.
	temp5 := self new.
	temp6 := Socket newTCP.
	temp6
		connectTo: temp7
		port: arg2.
	temp6 waitForConnectionFor: arg3.
	temp5 keepAliveTime: arg3.
	temp5 interface: arg4.
	temp5 pendingSocket: temp6.
	^ temp5.! !

!MQTTClientInterface methodsFor: 'initialization'!
initialize
	super initialize.
	keepAliveTime := 60.
	protocolVersion := 5.! !

!MQTTClientInterface methodsFor: 'accessing'!
hostName
	^ hostName.! !

!MQTTClientInterface methodsFor: 'accessing'!
hostName: arg1
	hostName := arg1.! !

!MQTTClientInterface methodsFor: 'accessing'!
keepAliveTime
	^ keepAliveTime.! !

!MQTTClientInterface methodsFor: 'accessing'!
keepAliveTime: arg1
	keepAliveTime := arg1.! !

!MQTTClientInterface methodsFor: 'accessing'!
protocolVersion
	^ protocolVersion.! !

!MQTTClientInterface methodsFor: 'accessing'!
protocolVersion: arg1
	protocolVersion := arg1.! !

!MQTTClientInterface methodsFor: 'accessing'!
socketClient
	^ socketClient.! !

!MQTTClientInterface methodsFor: 'accessing'!
socketClient: arg1
	socketClient := arg1.! !

!MQTTClientInterface methodsFor: 'accessing'!
socketClientClass
	^ MQTTSocketClient.! !

!MQTTClientInterface methodsFor: 'accessing'!
useVersion311
	protocolVersion := 4.! !

!MQTTClientInterface methodsFor: 'accessing'!
useVersion50
	protocolVersion := 5.! !

!MQTTClientInterface methodsFor: 'testing'!
isConnected
	^ self isValid.! !

!MQTTClientInterface methodsFor: 'testing'!
isValid
	^ (socketClient notNil and: [ socketClient transport notNil ]) and: [ socketClient transport abort not ].! !

!MQTTClientInterface methodsFor: 'testing'!
isVersion5
	^ protocolVersion = 5.! !

!MQTTClientInterface methodsFor: 'mqtt'!
clientID: arg1
	self socketClient clientID: arg1.! !

!MQTTClientInterface methodsFor: 'mqtt'!
connect
	self socketClient connect.! !

!MQTTClientInterface methodsFor: 'mqtt'!
debugLog: arg1
	self class debugLog: arg1.! !

!MQTTClientInterface methodsFor: 'mqtt'!
disconnect
	self socketClient ifNotNil: [ :temp1 |
		temp1 disconnect ].! !

!MQTTClientInterface methodsFor: 'mqtt'!
openOnHostName: arg1 port: arg2 keepAlive: arg3
	self hostName: arg1.
	self keepAliveTime: arg3.
	self socketClient:
		(self socketClientClass
			openOnHostName: arg1
			port: arg2
			keepAlive: arg3
			interface: self).! !

!MQTTClientInterface methodsFor: 'mqtt'!
publish: arg1 onTopic: arg2 qos: arg3
	self
		publish: arg1
		onTopic: arg2
		qos: arg3
		retain: false.! !

!MQTTClientInterface methodsFor: 'mqtt'!
publish: arg1 onTopic: arg2 qos: arg3 retain: arg4
	self socketClient
		publishTopic: arg2
		message: arg1
		qos: arg3
		retain: arg4.! !

!MQTTClientInterface methodsFor: 'mqtt'!
readWaitTime: arg1
	self socketClient readWaitTime: arg1.! !

!MQTTClientInterface methodsFor: 'mqtt'!
stop
	self disconnect.! !

!MQTTClientInterface methodsFor: 'mqtt'!
subscribeTo: arg1 qos: arg2 do: arg3
	self socketClient
		onTopic: arg1
		qos: arg2
		do: arg3.! !

!MQTTClientInterface methodsFor: 'mqtt'!
unsubscribeFrom: arg1
	self socketClient unsubscribeFrom: arg1.! !

!MQTTClientInterface methodsFor: 'mqtt'!
username: arg1 password: arg2
	self socketClient
		username: arg1
		password: arg2.! !

!MQTTClientInterface class methodsFor: 'instance creation'!
openOnHostName: arg1 port: arg2 keepAlive: arg3
	| temp4 temp5 |
	temp4 := self new.
	temp5 := arg3 > 0
		ifTrue: [ arg3 max: 60 ]
		ifFalse: [ 0 ].
	^ temp4
		openOnHostName: arg1
		port: arg2
		keepAlive: temp5.! !

!MQTTClientInterface class methodsFor: 'mqtt'!
debugLog: arg1
	self
		debugLog: arg1
		tag: ' CI '
		str2: ''.! !

!MQTTClientInterface class methodsFor: 'mqtt'!
debugLog: arg1 tag: arg2 str2: arg3
	| temp4 |
	temp4 := WriteStream on: (String new: 128).
	Time now
		print24: true
		showSeconds: true
		on: temp4.
	temp4
		nextPutAll: arg2;
		nextPutAll: arg1;
		space;
		nextPutAll: arg3.
	TranscriptMutex ifNil: [ TranscriptMutex := Semaphore forMutualExclusion ].
	TranscriptMutex critical: [
		Transcript
			show: temp4 contents;
			cr ].! !

!MQTTSubscription methodsFor: 'initialization'!
initialize
	super initialize.
	qos := 0.
	noLocal := false.
	retainAsPublished := false.
	retainHandling := 0.! !

!MQTTSubscription methodsFor: 'accessing'!
actionBlock
	^ actionBlock.! !

!MQTTSubscription methodsFor: 'accessing'!
actionBlock: arg1
	actionBlock := arg1.! !

!MQTTSubscription methodsFor: 'accessing'!
noLocal
	^ noLocal.! !

!MQTTSubscription methodsFor: 'accessing'!
noLocal: arg1
	noLocal := arg1.! !

!MQTTSubscription methodsFor: 'accessing'!
qos
	^ qos.! !

!MQTTSubscription methodsFor: 'accessing'!
qos: arg1
	qos := arg1.! !

!MQTTSubscription methodsFor: 'accessing'!
retainAsPublished
	^ retainAsPublished.! !

!MQTTSubscription methodsFor: 'accessing'!
retainAsPublished: arg1
	retainAsPublished := arg1.! !

!MQTTSubscription methodsFor: 'accessing'!
retainFlag
	^ retainFlag.! !

!MQTTSubscription methodsFor: 'accessing'!
retainFlag: arg1
	retainFlag := arg1.! !

!MQTTSubscription methodsFor: 'accessing'!
retainHandling
	^ retainHandling.! !

!MQTTSubscription methodsFor: 'accessing'!
retainHandling: arg1
	retainHandling := arg1.! !

!MQTTSubscription methodsFor: 'accessing'!
topic
	^ topic.! !

!MQTTSubscription methodsFor: 'accessing'!
topic: arg1
	topic := arg1.! !

!MQTTSubscription methodsFor: 'encoding'!
subscriptionOptionsV311
	^ qos bitAnd: 3.! !

!MQTTSubscription methodsFor: 'encoding'!
subscriptionOptionsV5
	| temp1 |
	temp1 := qos bitAnd: 3.
	noLocal ifTrue: [ temp1 := temp1 bitOr: 4 ].
	retainAsPublished ifTrue: [ temp1 := temp1 bitOr: 8 ].
	temp1 := temp1 bitOr: ((retainHandling bitAnd: 3) bitShift: 4).
	^ temp1.! !

!MQTTSubscription methodsFor: 'matching'!
matchParts: arg1 against: arg2 index: arg3
	| temp4 temp5 |
	arg3 > arg1 size ifTrue: [ ^ arg3 > arg2 size ].
	temp4 := arg1 at: arg3.
	temp4 = '#' ifTrue: [ ^ true ].
	arg3 > arg2 size ifTrue: [ ^ false ].
	temp5 := arg2 at: arg3.
	(temp4 = '+' or: [ temp4 = temp5 ]) ifTrue: [ ^ self
			matchParts: arg1
			against: arg2
			index: arg3 + 1 ].
	^ false.! !

!MQTTSubscription methodsFor: 'matching'!
topicMatches: arg1
	| temp2 temp3 |
	topic = arg1 ifTrue: [ ^ true ].
	((topic includes: $+) or: [ topic includes: $# ]) ifFalse: [ ^ false ].
	temp2 := topic findTokens: $/.
	temp3 := arg1 findTokens: $/.
	^ self
		matchParts: temp2
		against: temp3
		index: 1.! !

!MQTTSubscription class methodsFor: 'instance creation'!
for: arg1 qos: arg2 do: arg3
	^ self new
		topic: arg1;
		qos: arg2;
		actionBlock: arg3;
		yourself.! !

!MQTTPacketAndPendingJobPair methodsFor: 'accessing'!
packet
	^ packet.! !

!MQTTPacketAndPendingJobPair methodsFor: 'accessing'!
packet: arg1
	packet := arg1.! !

!MQTTPacketAndPendingJobPair methodsFor: 'accessing'!
pendingJob
	^ pendingJob.! !

!MQTTPacketAndPendingJobPair methodsFor: 'accessing'!
pendingJob: arg1
	pendingJob := arg1.! !

!MQTTPacketAndPendingJobPair class methodsFor: 'instance creation'!
packet: arg1 job: arg2
	^ self new
		packet: arg1;
		pendingJob: arg2;
		yourself.! !

!MQTTPendingJob methodsFor: 'initialization'!
initialize
	super initialize.
	sendTime := Time localSecondClock.! !

!MQTTPendingJob methodsFor: 'accessing'!
originalPacket
	^ originalPacket.! !

!MQTTPendingJob methodsFor: 'accessing'!
originalPacket: arg1
	originalPacket := arg1.! !

!MQTTPendingJob methodsFor: 'accessing'!
packetID
	^ originalPacket ifNotNil: [ originalPacket packetID ].! !

!MQTTPendingJob methodsFor: 'accessing'!
sendTime
	^ sendTime.! !

!MQTTPendingJob methodsFor: 'accessing'!
sendTime: arg1
	sendTime := arg1.! !

!MQTTPendingJob methodsFor: 'testing'!
isExpired: arg1
	^ Time localSecondClock - sendTime > arg1.! !

!MQTTPendingJob methodsFor: 'testing'!
isOverdue
	^ self isExpired: 30.! !

!MQTTPendingJob methodsFor: 'actions'!
resendOn: arg1
	originalPacket ifNotNil: [
		sendTime := Time localSecondClock.
		arg1 sendPacket: originalPacket ].! !

!MQTTPendingJob class methodsFor: 'instance creation'!
for: arg1
	^ self new
		originalPacket: arg1;
		yourself.! !
