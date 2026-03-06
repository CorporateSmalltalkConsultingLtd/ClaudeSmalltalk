'From Squeak6.0 [latest update: #22148] on 21 January 2026'!

"MCP Server for Smalltalk Agent — Squeak 6.0 Port (v3.0.0)

Implements the Model Context Protocol (MCP) over TCP with JSON-RPC 2.0.
Uses JSON Lines protocol (one JSON message per line).
Provides 14 tools for Smalltalk interaction.

v3.0.0 (JMM-658): TCP localhost transport with token authentication.
The VM is its own server — no external Python daemon needed.
Changes file always enabled (no playground mode).

REQUIRES: OSProcess package for environment variable access.
Install via: Installer ss project: 'OSProcess'; install: 'OSProcess'.

Usage:
  1. File in this code into a Squeak image
  2. Register startup: Smalltalk addToStartUpList: MCPServer
  3. Save the image
  4. Run: SMALLTALK_TCP_PORT=9876 SMALLTALK_TCP_TOKEN=<uuid> squeak image.image

The MCP Python server auto-starts the VM with these env vars set.
"!

SystemOrganization addCategory: #'MCP-Server'!

Object subclass: #MCPServer
	instanceVariableNames: 'running'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MCP-Server'!

!MCPServer commentStamp: 'jmm 3/5/2026 00:00' prior: 0!
MCP Server for Smalltalk Agent (v3.0.0).

Implements the Model Context Protocol (MCP) over TCP with JSON-RPC 2.0.
Provides 14 tools for Smalltalk code interaction.

Start with: SMALLTALK_TCP_PORT=9876 SMALLTALK_TCP_TOKEN=<uuid> squeak image.image!

Object subclass: #MCPTCPTransport
	instanceVariableNames: 'listenSocket port token running connections'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MCP-Server'!

!MCPTCPTransport commentStamp: 'jmm 3/5/2026 00:00' prior: 0!
TCP transport for MCP Server (JMM-658).
Listens on 127.0.0.1:port, authenticates connections with a shared token.
Each accepted connection is handled in its own Smalltalk process.
Replaces the Unix socket Python daemon — the VM is its own server.!

!MCPTCPTransport methodsFor: 'initialization' stamp: 'jmm 3/5/2026 00:00'!
initializePort: aPort token: aToken
	"Initialize TCP transport on 127.0.0.1:aPort with shared secret aToken."
	port := aPort.
	token := aToken.
	running := false.
	connections := OrderedCollection new.! !

!MCPTCPTransport methodsFor: 'running' stamp: 'jmm 3/5/2026 00:00'!
start
	"Start listening for TCP connections on 127.0.0.1:port.
	 Each authenticated connection gets its own MCPServer instance
	 running in a separate Smalltalk process."
	| clientSocket |
	listenSocket := Socket newTCP.
	listenSocket listenOn: port backlogSize: 5 interface: #[127 0 0 1].
	running := true.
	Transcript show: 'MCP TCP Server listening on 127.0.0.1:', port printString; cr.
	
	[	[running] whileTrue: [
			clientSocket := listenSocket waitForAcceptFor: 5.
			clientSocket ifNotNil: [
				Transcript show: 'MCP TCP: connection accepted'; cr.
				self handleConnection: clientSocket ]]
	] ensure: [
		listenSocket destroy.
		Transcript show: 'MCP TCP Server stopped'; cr ].! !

!MCPTCPTransport methodsFor: 'running' stamp: 'jmm 3/5/2026 00:00'!
stop
	"Stop the TCP server and close all connections."
	running := false.
	connections do: [:each | 
		[each close] on: Error do: [:e | "ignore"]].
	connections := OrderedCollection new.! !

!MCPTCPTransport methodsFor: 'connection handling' stamp: 'jmm 3/5/2026 00:00'!
handleConnection: clientSocket
	"Authenticate and then serve a TCP client connection.
	 Runs in its own forked process."
	[
		(self authenticate: clientSocket)
			ifTrue: [
				connections add: clientSocket.
				[self serveConnection: clientSocket]
					ensure: [
						connections remove: clientSocket ifAbsent: [].
						clientSocket destroy.
						Transcript show: 'MCP TCP: connection closed'; cr ]]
			ifFalse: [
				Transcript show: 'MCP TCP: authentication failed, closing'; cr.
				clientSocket destroy ]
	] forkAt: Processor userBackgroundPriority named: 'MCP TCP Client'.! !

!MCPTCPTransport methodsFor: 'connection handling' stamp: 'jmm 3/5/2026 00:00'!
authenticate: clientSocket
	"Read the first message and verify the token.
	 Expected: {jsonrpc:'2.0', method:'authenticate', params:{token:'...'}, id:N}
	 Responds with success or error, returns true/false."
	| line request method params clientToken id |
	
	"Wait for auth message with 10 second timeout"
	line := self readLineFrom: clientSocket timeout: 10.
	line ifNil: [ ^ false ].
	
	[request := Json readFrom: line readStream]
		on: Error do: [:e | ^ false].
	
	method := request at: 'method' ifAbsent: [''].
	id := request at: 'id' ifAbsent: [nil].
	method = 'authenticate' ifFalse: [ ^ false ].
	
	params := request at: 'params' ifAbsent: [Dictionary new].
	clientToken := params at: 'token' ifAbsent: [''].
	
	clientToken = token ifTrue: [
		"Send success response"
		id ifNotNil: [
			self writeLine: (Json render: (Dictionary new
				at: 'jsonrpc' put: '2.0';
				at: 'id' put: id;
				at: 'result' put: (Dictionary new at: 'authenticated' put: true; yourself);
				yourself))
			to: clientSocket ].
		^ true ].
	
	"Send error and reject"
	id ifNotNil: [
		self writeLine: (Json render: (Dictionary new
			at: 'jsonrpc' put: '2.0';
			at: 'id' put: id;
			at: 'error' put: (Dictionary new
				at: 'code' put: -32600;
				at: 'message' put: 'Authentication failed';
				yourself);
			yourself))
		to: clientSocket ].
	^ false.! !

!MCPTCPTransport methodsFor: 'connection handling' stamp: 'jmm 3/5/2026 00:00'!
serveConnection: clientSocket
	"Serve an authenticated TCP connection with an MCPServer instance.
	 Reads JSON-RPC requests, dispatches to MCPServer, writes responses."
	| server line request response |
	server := MCPServer new.
	
	[clientSocket isConnected] whileTrue: [
		line := self readLineFrom: clientSocket timeout: 300.
		line ifNil: [ ^ self "client disconnected" ].
		line isEmpty ifFalse: [
			[	request := Json readFrom: line readStream.
				response := server handleRequest: request.
				response ifNotNil: [
					self writeLine: (Json render: response) to: clientSocket ]
			] on: Error do: [:e |
				| errResp |
				errResp := server errorResponse: (request ifNotNil: [request at: 'id' ifAbsent: [nil]])
					code: -32603 message: e printString.
				self writeLine: (Json render: errResp) to: clientSocket ]]].! !

!MCPTCPTransport methodsFor: 'io' stamp: 'mcp 3/5/2026 16:35'!
readLineFrom: aSocket timeout: seconds
	"Read a line (terminated by LF) from a socket with timeout.
	 Returns nil if socket closed or timed out.
	 Socket>>receiveAvailableData returns a String (not ByteArray).
	 Socket>>waitForDataFor: signals exceptions, not Booleans.
	 Use waitForDataFor:ifClosed:ifTimedOut: with block callbacks."
	| stream data |
	stream := String new writeStream.
	
	[aSocket isConnected] whileTrue: [
		aSocket 
			waitForDataFor: seconds 
			ifClosed: [^ nil] 
			ifTimedOut: [^ nil].
		data := aSocket receiveAvailableData.
		data isEmpty ifTrue: [ ^ nil "closed" ].
		1 to: data size do: [:i |
			| char |
			char := data at: i.
			char = Character lf ifTrue: [ ^ stream contents ].
			char = Character cr ifFalse: [ stream nextPut: char ]]].
	^ nil "disconnected"! !

!MCPTCPTransport methodsFor: 'io' stamp: 'jmm 3/5/2026 00:00'!
writeLine: aString to: aSocket
	"Write a JSON line terminated by LF to a socket."
	| clean |
	clean := aString copyWithout: Character cr.
	clean := clean copyWithout: Character lf.
	aSocket sendData: clean, (String with: Character lf).! !

!MCPServer methodsFor: 'initialization' stamp: 'jmm 3/5/2026 00:00'!
initialize
	running := false.! !

!MCPServer methodsFor: 'request handling' stamp: 'Claude 1/21/2026 12:00'!
dispatch: method params: params
	"Dispatch JSON-RPC method to handler"
	method = 'initialize' ifTrue: [ ^ self handleInitialize: params ].
	method = 'tools/list' ifTrue: [ ^ self handleToolsList: params ].
	method = 'tools/call' ifTrue: [ ^ self handleToolsCall: params ].
	method = 'ping' ifTrue: [ ^ Dictionary new ].
	self error: 'Unknown method: ', method.! !

!MCPServer methodsFor: 'request handling' stamp: 'Claude 1/21/2026 12:00'!
handleNotification: method params: params
	"Handle JSON-RPC notifications (no response expected)"
	method = 'notifications/initialized' ifTrue: [ ^ self ].
	method = 'notifications/cancelled' ifTrue: [ ^ self ].! !

!MCPServer methodsFor: 'request handling' stamp: 'Claude 1/21/2026 12:00'!
handleRequest: request
	"Handle a JSON-RPC request, return response or nil for notifications"
	| method id result |
	method := request at: 'method' ifAbsent: [ ^ nil ].
	id := request at: 'id' ifAbsent: [ nil ].
	id ifNil: [
		self handleNotification: method
			params: (request at: 'params' ifAbsent: [ Dictionary new ]).
		^ nil ].
	[
		result := self dispatch: method
			params: (request at: 'params' ifAbsent: [ Dictionary new ]).
		^ self successResponse: id result: result
	] on: Error do: [ :ex |
		^ self errorResponse: id code: -32603 message: ex printString ].! !

!MCPServer methodsFor: 'responses' stamp: 'Claude 1/21/2026 12:00'!
errorResponse: id code: code message: message
	"Build a JSON-RPC error response"
	^ (Dictionary new)
		at: 'jsonrpc' put: '2.0';
		at: 'id' put: id;
		at: 'error' put: ((Dictionary new)
			at: 'code' put: code;
			at: 'message' put: message;
			yourself);
		yourself.! !

!MCPServer methodsFor: 'responses' stamp: 'Claude 1/21/2026 12:00'!
successResponse: id result: result
	"Build a JSON-RPC success response"
	^ (Dictionary new)
		at: 'jsonrpc' put: '2.0';
		at: 'id' put: id;
		at: 'result' put: result;
		yourself.! !

!MCPServer methodsFor: 'MCP handlers' stamp: 'Claude 1/21/2026 12:00'!
handleInitialize: params
	"Handle MCP initialize request"
	^ (Dictionary new)
		at: 'protocolVersion' put: '2024-11-05';
		at: 'capabilities' put: ((Dictionary new)
			at: 'tools' put: Dictionary new;
			yourself);
		at: 'serverInfo' put: ((Dictionary new)
			at: 'name' put: 'squeak-smalltalk';
			at: 'version' put: '1.0.0';
			yourself);
		yourself.! !

!MCPServer methodsFor: 'MCP handlers' stamp: 'Claude 1/21/2026 12:00'!
handleToolsCall: params
	"Handle MCP tools/call request"
	| name arguments resultText |
	name := params at: 'name'.
	arguments := params at: 'arguments' ifAbsent: [ Dictionary new ].
	resultText := self executeTool: name arguments: arguments.
	^ (Dictionary new)
		at: 'content' put: {
			(Dictionary new)
				at: 'type' put: 'text';
				at: 'text' put: resultText;
				yourself
		};
		yourself.! !

!MCPServer methodsFor: 'MCP handlers' stamp: 'Claude 1/21/2026 12:00'!
handleToolsList: params
	"Handle MCP tools/list request"
	^ (Dictionary new)
		at: 'tools' put: self toolDefinitions;
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: 'Claude 1/21/2026 12:00'!
toolDefBrowse
	^ (Dictionary new)
		at: 'name' put: 'smalltalk_browse';
		at: 'description' put: 'Browse a class to see its superclass, instance variables, class variables, and method selectors.';
		at: 'inputSchema' put: ((Dictionary new)
			at: 'type' put: 'object';
			at: 'properties' put: ((Dictionary new)
				at: 'className' put: ((Dictionary new)
					at: 'type' put: 'string';
					at: 'description' put: 'Name of the class to browse';
					yourself);
				yourself);
			at: 'required' put: #('className');
			yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: 'Claude 1/21/2026 12:00'!
toolDefClassesInCategory
	^ (Dictionary new)
		at: 'name' put: 'smalltalk_classes_in_category';
		at: 'description' put: 'List all classes in a specific category.';
		at: 'inputSchema' put: ((Dictionary new)
			at: 'type' put: 'object';
			at: 'properties' put: ((Dictionary new)
				at: 'category' put: ((Dictionary new)
					at: 'type' put: 'string';
					at: 'description' put: 'Name of the category';
					yourself);
				yourself);
			at: 'required' put: #('category');
			yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: 'Claude 1/21/2026 12:00'!
toolDefDefineClass
	^ (Dictionary new)
		at: 'name' put: 'smalltalk_define_class';
		at: 'description' put: 'Define a new class or modify an existing class definition.';
		at: 'inputSchema' put: ((Dictionary new)
			at: 'type' put: 'object';
			at: 'properties' put: ((Dictionary new)
				at: 'definition' put: ((Dictionary new)
					at: 'type' put: 'string';
					at: 'description' put: 'Full class definition expression';
					yourself);
				yourself);
			at: 'required' put: #('definition');
			yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: 'Claude 1/21/2026 12:00'!
toolDefDefineMethod
	^ (Dictionary new)
		at: 'name' put: 'smalltalk_define_method';
		at: 'description' put: 'Define or modify a method on a class.';
		at: 'inputSchema' put: ((Dictionary new)
			at: 'type' put: 'object';
			at: 'properties' put: ((Dictionary new)
				at: 'className' put: ((Dictionary new)
					at: 'type' put: 'string';
					at: 'description' put: 'Name of the class';
					yourself);
				at: 'source' put: ((Dictionary new)
					at: 'type' put: 'string';
					at: 'description' put: 'Full method source including selector';
					yourself);
				yourself);
			at: 'required' put: #('className' 'source');
			yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: 'Claude 1/21/2026 12:00'!
toolDefDeleteClass
	^ (Dictionary new)
		at: 'name' put: 'smalltalk_delete_class';
		at: 'description' put: 'Remove a class from the system.';
		at: 'inputSchema' put: ((Dictionary new)
			at: 'type' put: 'object';
			at: 'properties' put: ((Dictionary new)
				at: 'className' put: ((Dictionary new)
					at: 'type' put: 'string';
					at: 'description' put: 'Name of the class to remove';
					yourself);
				yourself);
			at: 'required' put: #('className');
			yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: 'Claude 1/21/2026 12:00'!
toolDefDeleteMethod
	^ (Dictionary new)
		at: 'name' put: 'smalltalk_delete_method';
		at: 'description' put: 'Remove a method from a class.';
		at: 'inputSchema' put: ((Dictionary new)
			at: 'type' put: 'object';
			at: 'properties' put: ((Dictionary new)
				at: 'className' put: ((Dictionary new)
					at: 'type' put: 'string';
					at: 'description' put: 'Name of the class';
					yourself);
				at: 'selector' put: ((Dictionary new)
					at: 'type' put: 'string';
					at: 'description' put: 'Selector of the method to remove';
					yourself);
				yourself);
			at: 'required' put: #('className' 'selector');
			yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: 'Claude 1/21/2026 12:00'!
toolDefEvaluate
	^ (Dictionary new)
		at: 'name' put: 'smalltalk_evaluate';
		at: 'description' put: 'Evaluate arbitrary Smalltalk code and return the result.';
		at: 'inputSchema' put: ((Dictionary new)
			at: 'type' put: 'object';
			at: 'properties' put: ((Dictionary new)
				at: 'code' put: ((Dictionary new)
					at: 'type' put: 'string';
					at: 'description' put: 'Smalltalk code to evaluate';
					yourself);
				yourself);
			at: 'required' put: #('code');
			yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: 'Claude 1/21/2026 12:00'!
toolDefHierarchy
	^ (Dictionary new)
		at: 'name' put: 'smalltalk_hierarchy';
		at: 'description' put: 'Get the inheritance hierarchy for a class (from Object down to the class).';
		at: 'inputSchema' put: ((Dictionary new)
			at: 'type' put: 'object';
			at: 'properties' put: ((Dictionary new)
				at: 'className' put: ((Dictionary new)
					at: 'type' put: 'string';
					at: 'description' put: 'Name of the class';
					yourself);
				yourself);
			at: 'required' put: #('className');
			yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: 'Claude 1/21/2026 12:00'!
toolDefListCategories
	^ (Dictionary new)
		at: 'name' put: 'smalltalk_list_categories';
		at: 'description' put: 'List all system categories.';
		at: 'inputSchema' put: ((Dictionary new)
			at: 'type' put: 'object';
			at: 'properties' put: Dictionary new;
			yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: 'Claude 1/21/2026 12:00'!
toolDefListClasses
	^ (Dictionary new)
		at: 'name' put: 'smalltalk_list_classes';
		at: 'description' put: 'List all classes in the system, optionally filtered by prefix.';
		at: 'inputSchema' put: ((Dictionary new)
			at: 'type' put: 'object';
			at: 'properties' put: ((Dictionary new)
				at: 'prefix' put: ((Dictionary new)
					at: 'type' put: 'string';
					at: 'description' put: 'Optional prefix to filter class names';
					yourself);
				yourself);
			yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: 'Claude 1/21/2026 12:00'!
toolDefMethodSource
	^ (Dictionary new)
		at: 'name' put: 'smalltalk_method_source';
		at: 'description' put: 'Get the source code of a specific method. Use side=class for class-side methods, or append " class" to className.';
		at: 'inputSchema' put: ((Dictionary new)
			at: 'type' put: 'object';
			at: 'properties' put: ((Dictionary new)
				at: 'className' put: ((Dictionary new)
					at: 'type' put: 'string';
					at: 'description' put: 'Name of the class (append " class" for class-side, e.g. "MCPServer class")';
					yourself);
				at: 'selector' put: ((Dictionary new)
					at: 'type' put: 'string';
					at: 'description' put: 'Method selector';
					yourself);
				at: 'side' put: ((Dictionary new)
					at: 'type' put: 'string';
					at: 'enum' put: #('instance' 'class');
					at: 'description' put: 'Which side to look up: instance (default) or class';
					yourself);
				yourself);
			at: 'required' put: #('className' 'selector');
			yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: 'Claude 1/21/2026 12:00'!
toolDefSubclasses
	^ (Dictionary new)
		at: 'name' put: 'smalltalk_subclasses';
		at: 'description' put: 'Get the direct subclasses of a class.';
		at: 'inputSchema' put: ((Dictionary new)
			at: 'type' put: 'object';
			at: 'properties' put: ((Dictionary new)
				at: 'className' put: ((Dictionary new)
					at: 'type' put: 'string';
					at: 'description' put: 'Name of the class';
					yourself);
				yourself);
			at: 'required' put: #('className');
			yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: 'jmm 1/29/2026 20:20'!
toolDefSaveImage
	^ (Dictionary new)
		at: 'name' put: 'smalltalk_save_image';
		at: 'description' put: 'Save the current image in place. Only available in dev mode.';
		at: 'inputSchema' put: ((Dictionary new)
			at: 'type' put: 'object';
			at: 'properties' put: Dictionary new;
			yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: 'jmm 1/29/2026 20:20'!
toolDefSaveAsNewVersion
	^ (Dictionary new)
		at: 'name' put: 'smalltalk_save_as_new_version';
		at: 'description' put: 'Save the image/changes using the next available version number. Only available in dev mode.';
		at: 'inputSchema' put: ((Dictionary new)
			at: 'type' put: 'object';
			at: 'properties' put: Dictionary new;
			yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: 'Claude 1/21/2026 12:00'!
toolDefinitions
	"Return all tool definitions for MCP tools/list"
	^ {
		self toolDefEvaluate.
		self toolDefBrowse.
		self toolDefMethodSource.
		self toolDefDefineClass.
		self toolDefDefineMethod.
		self toolDefDeleteMethod.
		self toolDefDeleteClass.
		self toolDefListClasses.
		self toolDefHierarchy.
		self toolDefSubclasses.
		self toolDefListCategories.
		self toolDefClassesInCategory.
		self toolDefSaveImage.
		self toolDefSaveAsNewVersion
	}.! !

!MCPServer methodsFor: 'tool dispatch' stamp: 'Claude 1/21/2026 12:00'!
executeTool: name arguments: args
	"Dispatch tool call to implementation"
	name = 'smalltalk_evaluate' ifTrue: [ ^ self toolEvaluate: args ].
	name = 'smalltalk_browse' ifTrue: [ ^ self toolBrowse: args ].
	name = 'smalltalk_method_source' ifTrue: [ ^ self toolMethodSource: args ].
	name = 'smalltalk_define_class' ifTrue: [ ^ self toolDefineClass: args ].
	name = 'smalltalk_define_method' ifTrue: [ ^ self toolDefineMethod: args ].
	name = 'smalltalk_delete_method' ifTrue: [ ^ self toolDeleteMethod: args ].
	name = 'smalltalk_delete_class' ifTrue: [ ^ self toolDeleteClass: args ].
	name = 'smalltalk_list_classes' ifTrue: [ ^ self toolListClasses: args ].
	name = 'smalltalk_hierarchy' ifTrue: [ ^ self toolHierarchy: args ].
	name = 'smalltalk_subclasses' ifTrue: [ ^ self toolSubclasses: args ].
	name = 'smalltalk_list_categories' ifTrue: [ ^ self toolListCategories: args ].
	name = 'smalltalk_classes_in_category' ifTrue: [ ^ self toolClassesInCategory: args ].
	name = 'smalltalk_save_image' ifTrue: [ ^ self toolSaveImage: args ].
	name = 'smalltalk_save_as_new_version' ifTrue: [ ^ self toolSaveAsNewVersion: args ].
	self error: 'Unknown tool: ', name.! !

!MCPServer methodsFor: 'tool implementations' stamp: 'Claude 1/21/2026 12:00'!
toolBrowse: args
	"Browse a class - return metadata as JSON.
	 Includes both instance-side and class-side methods."
	| className class result |
	className := args at: 'className' ifAbsent: [ '' ].
	className isEmpty ifTrue: [ self error: 'No className provided' ].
	class := Smalltalk at: className asSymbol ifAbsent: [
		self error: 'Class not found: ', className ].
	result := Dictionary new.
	result at: 'name' put: class name.
	result at: 'superclass' put: (class superclass
		ifNil: [ 'nil' ]
		ifNotNil: [ :sc | sc name ]).
	result at: 'category' put: class category.
	result at: 'instanceVariables' put: class instVarNames asArray.
	result at: 'classVariables' put: class classVarNames asArray.
	result at: 'methods' put: class selectors asArray sorted.
	result at: 'classMethods' put: class class selectors asArray sorted.
	result at: 'comment' put: ([ class comment ifNil: [ '' ] ] on: Error do: [ :e | '' ]).
	^ Json render: result.! !

!MCPServer methodsFor: 'tool implementations' stamp: 'Claude 1/21/2026 12:00'!
toolClassesInCategory: args
	"List all classes in a category"
	| category classes names |
	category := args at: 'category' ifAbsent: [ '' ].
	category isEmpty ifTrue: [ self error: 'No category provided' ].
	classes := Smalltalk allClasses select: [ :c |
		c category = category asSymbol ].
	names := (classes collect: [ :c | c name ]) asArray sorted.
	^ Json render: names.! !

!MCPServer methodsFor: 'tool implementations' stamp: 'Claude 1/21/2026 12:00'!
toolDefineClass: args
	"Define a new class.
	 Squeak requires poolDictionaries: in the definition; inject it if the LLM omits it."
	| definition |
	definition := args at: 'definition' ifAbsent: [ '' ].
	definition isEmpty ifTrue: [ self error: 'No definition provided' ].
	"Auto-inject poolDictionaries: '' before category: if missing"
	((definition includesSubString: 'poolDictionaries:') not
		and: [ definition includesSubString: 'category:' ]) ifTrue: [
		definition := definition copyReplaceAll: 'category:' with: 'poolDictionaries: '''' category:' ].
	Compiler evaluate: definition.
	^ 'Class defined successfully'.! !

!MCPServer methodsFor: 'tool implementations' stamp: 'Claude 1/21/2026 12:00'!
toolDefineMethod: args
	"Define or modify a method on a class.
	 Always uses compile:classified: to write source to .changes file.
	 (JMM-658: playground mode removed, changes always enabled)"
	| className source category class |
	className := args at: 'className' ifAbsent: [ '' ].
	source := args at: 'source' ifAbsent: [ '' ].
	category := args at: 'category' ifAbsent: [ 'as yet unclassified' ].
	className isEmpty ifTrue: [ self error: 'No className provided' ].
	source isEmpty ifTrue: [ self error: 'No source provided' ].
	class := Smalltalk at: className asSymbol ifAbsent: [
		self error: 'Class not found: ', className ].
	class compile: source classified: category.
	^ 'Method defined successfully'.! !

!MCPServer methodsFor: 'tool implementations' stamp: 'Claude 1/21/2026 12:00'!
toolDeleteClass: args
	"Remove a class from the system"
	| className class |
	className := args at: 'className' ifAbsent: [ '' ].
	className isEmpty ifTrue: [ self error: 'No className provided' ].
	class := Smalltalk at: className asSymbol ifAbsent: [
		self error: 'Class not found: ', className ].
	class removeFromSystem.
	^ 'Class deleted successfully'.! !

!MCPServer methodsFor: 'tool implementations' stamp: 'Claude 1/21/2026 12:00'!
toolDeleteMethod: args
	"Remove a method from a class"
	| className selector class |
	className := args at: 'className' ifAbsent: [ '' ].
	selector := args at: 'selector' ifAbsent: [ '' ].
	className isEmpty ifTrue: [ self error: 'No className provided' ].
	selector isEmpty ifTrue: [ self error: 'No selector provided' ].
	class := Smalltalk at: className asSymbol ifAbsent: [
		self error: 'Class not found: ', className ].
	(class includesSelector: selector asSymbol) ifFalse: [
		self error: 'Method not found: ', className, '>>', selector ].
	class removeSelector: selector asSymbol.
	^ 'Method deleted successfully'.! !

!MCPServer methodsFor: 'tool implementations' stamp: 'Claude 1/21/2026 12:00'!
toolEvaluate: args
	"Evaluate Smalltalk code and return result"
	| code result |
	code := args at: 'code' ifAbsent: [ '' ].
	code isEmpty ifTrue: [ self error: 'No code provided' ].
	result := Compiler evaluate: code.
	^ result printString.! !

!MCPServer methodsFor: 'tool implementations' stamp: 'Claude 1/21/2026 12:00'!
toolHierarchy: args
	"Get inheritance hierarchy from Object down to this class"
	| className class hierarchy |
	className := args at: 'className' ifAbsent: [ '' ].
	className isEmpty ifTrue: [ self error: 'No className provided' ].
	class := Smalltalk at: className asSymbol ifAbsent: [
		self error: 'Class not found: ', className ].
	hierarchy := OrderedCollection new.
	[ class notNil ] whileTrue: [
		hierarchy add: class name.
		class := class superclass ].
	^ Json render: hierarchy asArray.! !

!MCPServer methodsFor: 'tool implementations' stamp: 'Claude 1/21/2026 12:00'!
toolListCategories: args
	"List all system categories"
	| categories |
	categories := SystemOrganization categories asArray sorted.
	^ Json render: categories.! !

!MCPServer methodsFor: 'tool implementations' stamp: 'Claude 1/21/2026 12:00'!
toolListClasses: args
	"List all classes, optionally filtered by prefix"
	| prefix classes names |
	prefix := args at: 'prefix' ifAbsent: [ '' ].
	classes := Smalltalk allClasses.
	prefix isEmpty ifFalse: [
		classes := classes select: [ :c |
			c name asString beginsWith: prefix ]].
	names := (classes collect: [ :c | c name ]) asArray sorted.
	^ Json render: names.! !

!MCPServer methodsFor: 'tool implementations' stamp: 'Claude 1/21/2026 12:00'!
toolMethodSource: args
	"Get source code of a method. Supports both instance and class side.
	 Class side can be requested via side='class' parameter or by appending
	 ' class' to className (e.g. 'MCPServer class')."
	| className selector side class target method displayName |
	className := args at: 'className' ifAbsent: [ '' ].
	selector := args at: 'selector' ifAbsent: [ '' ].
	side := args at: 'side' ifAbsent: [ 'instance' ].
	className isEmpty ifTrue: [ self error: 'No className provided' ].
	selector isEmpty ifTrue: [ self error: 'No selector provided' ].
	
	"Handle 'ClassName class' syntax in className"
	(className endsWith: ' class') ifTrue: [
		className := className copyFrom: 1 to: className size - 6.
		side := 'class' ].
	
	class := Smalltalk at: className asSymbol ifAbsent: [
		self error: 'Class not found: ', className ].
	
	target := side = 'class'
		ifTrue: [ class class ]
		ifFalse: [ class ].
	displayName := side = 'class'
		ifTrue: [ className, ' class' ]
		ifFalse: [ className ].
	
	(target includesSelector: selector asSymbol) ifFalse: [
		self error: 'Method not found: ', displayName, '>>', selector ].
	method := target >> selector asSymbol.
	"Try source file first, fall back to decompilation if .changes is unavailable"
	^ [ method getSourceFromFile asString ]
		on: Error
		do: [ :e | method decompileString ].! !

!MCPServer methodsFor: 'tool implementations' stamp: 'Claude 1/21/2026 12:00'!
toolSubclasses: args
	"Get direct subclasses of a class"
	| className class names |
	className := args at: 'className' ifAbsent: [ '' ].
	className isEmpty ifTrue: [ self error: 'No className provided' ].
	class := Smalltalk at: className asSymbol ifAbsent: [
		self error: 'Class not found: ', className ].
	names := (class subclasses collect: [ :c | c name ]) asArray sorted.
	^ Json render: names.! !

!MCPServer methodsFor: 'tool implementations' stamp: 'jmm 1/29/2026 21:00'!
toolSaveImage: args
	"Save the current image in place.
	Uses headlessSave which skips Cursor/Morphic UI operations."
	self class headlessSave.
	^ 'Image saved: ', Smalltalk imageName.! !

!MCPServer methodsFor: 'tool implementations' stamp: 'jmm 1/29/2026 21:00'!
toolSaveAsNewVersion: args
	"Save image/changes as next version number.
	Uses headlessSaveAsNewVersion to avoid UI hangs under xvfb."
	self class headlessSaveAsNewVersion.
	^ 'Saved as new version: ', Smalltalk imageName.! !

!MCPServer class methodsFor: 'system startup' stamp: 'jmm 1/30/2026 03:45'!
getenv: varName
	"Portable environment variable access.
	 Cuis has Smalltalk getenv: built in.
	 Squeak 6.0 does not — must use OSProcess.
	 Returns the value as a String, or nil if not set.
	 
	 Note: In startUp:, this is called BEFORE OSProcess session refresh.
	 That is safe because OSProcess environment access reads /proc/self/environ
	 (or equivalent) which does not depend on session state."
	
	(Smalltalk respondsTo: #getenv:) ifTrue: [
		^ Smalltalk getenv: varName ].
	
	"Squeak with OSProcess"
	^ [ | env |
		env := OSProcess thisOSProcess environment.
		env at: varName ifAbsent: [ nil ] ]
	on: Error do: [ :e | nil ].! !

!MCPServer class methodsFor: 'system startup' stamp: 'jmm 3/5/2026 00:00'!
getArgValue: argName
	"Get the value following a command line argument.
	 e.g. for '--port 9876', (self getArgValue: '--port') returns '9876'.
	 Returns nil if the argument is not found or has no value."
	| args idx |
	args := Smalltalk arguments.
	idx := args indexOf: argName.
	(idx > 0 and: [idx < args size])
		ifTrue: [ ^ args at: idx + 1 ]
		ifFalse: [ ^ nil ].! !

!MCPServer class methodsFor: 'system startup' stamp: 'jmm 3/6/2026 00:00'!
startUp: resuming
	"Called on image startup.
	 Starts TCP server if --tcp flag is present OR if SMALLTALK_TCP_PORT
	 env var is set. Env vars allow launching without positional CLI args,
	 which avoids the macOS VM treating --tcp as a script filename."
	resuming ifFalse: [ ^ self ].
	
	((Smalltalk arguments includes: '--tcp') or: [
		(self getenv: 'SMALLTALK_TCP_PORT') notNil]) ifTrue: [
		self startTCPServer ].! !

!MCPServer class methodsFor: 'version' stamp: 'jmm 1/30/2026 03:45'!
version
	"Return the MCP server version number.
	 Increment when making changes that external tools should detect.
	 Version history:
	   1 - Initial version
	   2 - Fixed toolDefineMethod: to use compileSilently: for headless operation
	   3 - JMM-515: OSProcess session refresh fix for MCP stdin/stdout
	   4 - JMM-512: Dev mode with save_image and save_as_new_version tools
	   5 - JMM-515: Daemon mode via startUp: (no --doit needed)
	   6 - JMM-515: Fix getenv: for Squeak (Cuis-only method)
	   7 - JMM-509: Class-side method support in browse and method-source
	   8 - JMM-656: toolDefineMethod uses compile:classified: in dev mode for .changes file
	   9 - JMM-658: TCP transport, token auth, no playground mode (v3.0.0)
	  10 - JMM-662: Fix MorphicProject>>launchSystemFiles:event: crash on macOS startup
	  11 - JMM-662: startUp:/startTCPServer fall back to SMALLTALK_TCP_PORT/TOKEN env vars"
	^ 11! !

!MCPServer class methodsFor: 'instance creation' stamp: 'jmm 3/5/2026 00:00'!
startTCPServer
	"Start the MCP TCP server (v3.0.0).
	 Listens on 127.0.0.1:port with token authentication.
	 Changes file always enabled — no playground mode.
	 
	 Config via environment variables (both macOS and Linux):
	   SMALLTALK_TCP_PORT   (required)
	   SMALLTALK_TCP_TOKEN  (required)
	   SMALLTALK_TCP_HOST   (always 127.0.0.1)
	 
	 Usage:  SMALLTALK_TCP_PORT=9876 SMALLTALK_TCP_TOKEN=<uuid> squeak image.image
	 
	 CLI args (--tcp --port --token) are still accepted for manual invocation
	 but env vars are preferred — the MCP Python server always uses env vars.
	 
	 The TCP server runs in a background process, allowing the
	 Squeak GUI to remain responsive."
	| portStr port tokenStr transport |
	
	"Refresh OSProcess handles for new VM session"
	OSProcess thisOSProcess refreshFromProcessAccessor.
	
	portStr := self getArgValue: '--port'.
	(portStr isNil or: [portStr isEmpty]) ifTrue: [
		portStr := self getenv: 'SMALLTALK_TCP_PORT' ].
	(portStr isNil or: [portStr isEmpty]) ifTrue: [
		Transcript show: 'MCP TCP Server: --port arg or SMALLTALK_TCP_PORT env var required'; cr.
		^ self ].
	port := portStr asInteger.
	
	tokenStr := self getArgValue: '--token'.
	(tokenStr isNil or: [tokenStr isEmpty]) ifTrue: [
		tokenStr := self getenv: 'SMALLTALK_TCP_TOKEN' ].
	(tokenStr isNil or: [tokenStr isEmpty]) ifTrue: [
		Transcript show: 'MCP TCP Server: --token arg or SMALLTALK_TCP_TOKEN env var required'; cr.
		^ self ].
	
	"Set author initials to avoid prompt during class/method definition"
	Utilities authorInitialsPerSe isEmpty ifTrue: [
		Utilities setAuthorInitials: 'mcp' ].
	
	Transcript show: 'MCP TCP Server v3.0.0 starting on 127.0.0.1:', port printString; cr.
	
	transport := MCPTCPTransport new initializePort: port token: tokenStr.
	
	[transport start] 
		forkAt: Processor userBackgroundPriority 
		named: 'MCP TCP Server'.! !

!MCPServer class methodsFor: 'saving' stamp: 'jmm 1/29/2026 21:10'!
headlessSaveAsNewVersion
	"Save image/changes as next version number, headlessly.
	Based on SmalltalkImage>>saveAsNewVersion but using headlessSave
	instead of snapshot:andQuit: to avoid UI hangs."
	
	| baseName dotIndex newName newChanges |
	
	baseName := FileDirectory baseNameFor: 
		(FileDirectory default localNameFor: Smalltalk imageName).
	dotIndex := baseName lastIndexOf: FileDirectory dot asCharacter
		ifAbsent: [ nil ].
	(dotIndex notNil and: [ (baseName copyFrom: dotIndex + 1 to: baseName size) isAllDigits ])
		ifTrue: [ baseName := baseName copyFrom: 1 to: dotIndex - 1 ].
	
	newName := FileDirectory default nextNameFor: baseName 
		extension: FileDirectory imageSuffix.
	newChanges := Smalltalk fullNameForChangesNamed: newName.
	
	(FileDirectory default fileOrDirectoryExists: newChanges)
		ifTrue: [ self error: 'Changes file already exists: ', newChanges ].
	
	"Close current sources, copy changes, reopen with new name"
	(SourceFiles at: 2) ifNotNil: [
		Smalltalk closeSourceFiles.
		Smalltalk saveChangesInFileNamed: (Smalltalk fullNameForChangesNamed: newName) ].
	
	"Rename image to new version and save"
	Smalltalk changeImageNameTo: 
		(FileDirectory default fullNameFor: (Smalltalk fullNameForImageNamed: newName)).
	Smalltalk openSourceFiles.
	
	self headlessSave.! !

!MCPServer class methodsFor: 'saving' stamp: 'jmm 1/29/2026 21:00'!
headlessSave
	"Save the image without Cursor/Morphic operations.
	Based on SmalltalkImage>>snapshot:andQuit:withExitCode:embedded: but
	stripped of UI entanglements that hang under xvfb-run:
	  - No Cursor write/normal show
	  - No Project current wakeUpTopWindow
	  - No processShutDownList (we are not quitting)
	  - No startUpPostSnapshot (we are not resuming from snapshot)
	Changes file logging IS preserved for traceability."
	
	| changesLog result |
	
	"Flush weak references"
	Object flushDependents.
	Object flushEvents.
	
	"Log to changes file"
	(SourceFiles at: 2) ifNotNil: [ :changesFile |
		changesLog := String streamContents: [ :s |
			s nextPutAll: '----SNAPSHOT----';
			  print: Date dateAndTimeNow;
			  space;
			  nextPutAll: (FileDirectory default localNameFor: Smalltalk imageName);
			  nextPutAll: ' priorSource: ';
			  print: Smalltalk lastQuitLogPosition ].
		Smalltalk assureStartupStampLogged.
		Smalltalk lastQuitLogPosition: (SourceFiles at: 2) setToEnd position.
		Smalltalk logChange: changesLog.
		Transcript cr; show: changesLog ].
	
	"Save"
	result := Smalltalk snapshotPrimitive.
	
	result ifNil: [ self error: 'Failed to write image file (disk full?)' ].
	
	"After save, result is false (continuing from save point).
	 If image is later restored, result would be true (resuming)."
	^ result! !

!MorphicProject methodsFor: 'launching' stamp: 'jmm 3/6/2026 00:00'!
launchSystemFiles: fileStreams event: genericMorphicEvent
	"Handle a number of files the singleton VM was invoked with again.
	 Guard against empty fileStreams: on macOS the VM sends this event when
	 launched with an image path argument, but fileStreams may be empty
	 if the image is already open (it can't be re-opened as a stream)."
	fileStreams isEmpty ifTrue: [ ^ self ].
	self flag: #todo. "Do something more useful with the image here, e. g. tell the VM to load it."
	self inform: ('Cannot start a second instance of Squeak\with the image "{1}"\because the VM is configured as singleton application.' translated withCRs format: {fileStreams first localName}).! !

"Setup (in a Workspace, do it):
  Installer ss project: 'OSProcess'; install: 'OSProcess'.
Then file in this .st file, then:
  Smalltalk addToStartUpList: MCPServer.
  Smalltalk saveAs: 'ClaudeSqueak'.
Launch with:
  SMALLTALK_TCP_PORT=9876 SMALLTALK_TCP_TOKEN=<uuid> squeak ClaudeSqueak.image
"!
