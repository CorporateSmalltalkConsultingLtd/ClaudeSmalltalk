'From Squeak6.0 [latest update: #22148] on 21 January 2026'!

"MCP Server for Claude Code integration - Squeak 6.0 Port

Implements the Model Context Protocol (MCP) over stdio with JSON-RPC 2.0.
Uses JSON Lines protocol (one JSON message per line, no Content-Length headers).
Provides 12 tools for Smalltalk interaction (saveImage intentionally excluded from MCP).

REQUIRES: OSProcess package for async stdio support.
Install via: Installer squeakMap install: 'OSProcess'.

Uses BufferedAsyncFileReadStream for stdin which provides semaphore-based
waiting, allowing the GUI to remain responsive while the MCP server waits
for input from Claude Code.

Usage:
  1. File in this code into a Squeak image
  2. Register startup: Smalltalk addToStartUpList: MCPServer
  3. Save the image
  4. Run with --mcp flag:
     squeak myImage.image --mcp

Claude Code config (~/.claude.json):
  {
    'mcpServers': {
      'squeakDirect': {
        'type': 'stdio',
        'command': '/path/to/Squeak',
        'args': ['/path/to/ClaudeMCP-Squeak.image', '--mcp']
      }
    }
  }
"!

SystemOrganization addCategory: #'MCP-Server'!

Object subclass: #MCPServer
	instanceVariableNames: 'transport running'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MCP-Server'!

!MCPServer commentStamp: 'Claude 1/21/2026 12:00' prior: 0!
MCP Server for Claude Code integration.

Implements the Model Context Protocol (MCP) over stdio with JSON-RPC 2.0.
Provides 12 tools for Smalltalk code interaction.

Start the server by running the image with --mcp flag.

Uses BufferedAsyncFileReadStream for responsive GUI while waiting for input.!

Object subclass: #MCPTransport
	instanceVariableNames: 'stdin stdout'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MCP-Server'!

!MCPTransport commentStamp: 'Claude 1/21/2026 12:00' prior: 0!
Transport layer for MCP using JSON Lines protocol over stdio.

Uses OSProcess BufferedAsyncFileReadStream for stdin which provides
semaphore-based waiting. This allows the VM scheduler to run other
processes (including the UI event loop) while waiting for input.!

!MCPTransport methodsFor: 'initialization' stamp: 'Claude 1/21/2026 12:00'!
initialize
	"Initialize using OSProcess for stdio with buffered async reads.
	BufferedAsyncFileReadStream uses semaphore-based waiting which
	allows the VM to schedule other processes (like the UI) while waiting for input."
	stdin := OSProcess thisOSProcess stdIn asBufferedAsyncFileReadStream.
	stdin setBlocking.  "Use blocking mode with semaphore wait"
	stdout := OSProcess thisOSProcess stdOut.! !

!MCPTransport methodsFor: 'reading' stamp: 'Claude 1/21/2026 12:00'!
readLine
	"Read a line from stdin using buffered async stream.
	Uses semaphore-based waiting which allows UI to remain responsive."
	| stream char |
	stream := String new writeStream.
	[
		char := stdin next.
		char isNil ifTrue: [ ^ nil ].
		(char = Character cr or: [ char = Character lf ])
			ifTrue: [ ^ stream contents ].
		stream nextPut: char
	] repeat.! !

!MCPTransport methodsFor: 'reading' stamp: 'Claude 1/21/2026 12:00'!
readMessage
	"Read a JSON message from stdin"
	| line |
	line := self readLine.
	line isNil ifTrue: [ ^ nil ].
	line isEmpty ifTrue: [ ^ self readMessage ].
	^ Json readFrom: line readStream.! !

!MCPTransport methodsFor: 'writing' stamp: 'Claude 1/21/2026 12:00'!
writeMessage: jsonString
	"Write a JSON message to stdout, ensuring no embedded newlines"
	| clean |
	clean := jsonString copyWithout: Character cr.
	clean := clean copyWithout: Character lf.
	stdout
		nextPutAll: clean;
		nextPut: Character lf;
		flush.! !

!MCPServer methodsFor: 'initialization' stamp: 'Claude 1/21/2026 12:00'!
initialize
	transport := MCPTransport new.
	running := false.! !

!MCPServer methodsFor: 'running' stamp: 'Claude 1/21/2026 12:00'!
run
	"Main server loop - read requests, dispatch, write responses"
	| request response |
	running := true.
	[ running ] whileTrue: [
		request := transport readMessage.
		request
			ifNil: [ running := false ]
			ifNotNil: [
				response := self handleRequest: request.
				response ifNotNil: [
					transport writeMessage: (Json render: response) ]]].! !

!MCPServer methodsFor: 'running' stamp: 'Claude 1/21/2026 12:00'!
stop
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
		at: 'description' put: 'Get the source code of a specific method.';
		at: 'inputSchema' put: ((Dictionary new)
			at: 'type' put: 'object';
			at: 'properties' put: ((Dictionary new)
				at: 'className' put: ((Dictionary new)
					at: 'type' put: 'string';
					at: 'description' put: 'Name of the class';
					yourself);
				at: 'selector' put: ((Dictionary new)
					at: 'type' put: 'string';
					at: 'description' put: 'Method selector';
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
		self toolDefClassesInCategory
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
	self error: 'Unknown tool: ', name.! !

!MCPServer methodsFor: 'tool implementations' stamp: 'Claude 1/21/2026 12:00'!
toolBrowse: args
	"Browse a class - return metadata as JSON"
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
	result at: 'comment' put: (class comment ifNil: [ '' ]).
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
	"Define a new class"
	| definition |
	definition := args at: 'definition' ifAbsent: [ '' ].
	definition isEmpty ifTrue: [ self error: 'No definition provided' ].
	Compiler evaluate: definition.
	^ 'Class defined successfully'.! !

!MCPServer methodsFor: 'tool implementations' stamp: 'Claude 1/21/2026 12:00'!
toolDefineMethod: args
	"Define or modify a method on a class"
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
	"Get source code of a method"
	| className selector class method |
	className := args at: 'className' ifAbsent: [ '' ].
	selector := args at: 'selector' ifAbsent: [ '' ].
	className isEmpty ifTrue: [ self error: 'No className provided' ].
	selector isEmpty ifTrue: [ self error: 'No selector provided' ].
	class := Smalltalk at: className asSymbol ifAbsent: [
		self error: 'Class not found: ', className ].
	(class includesSelector: selector asSymbol) ifFalse: [
		self error: 'Method not found: ', className, '>>', selector ].
	method := class >> selector asSymbol.
	^ method getSourceFromFile asString.! !

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

!MCPServer class methodsFor: 'system startup' stamp: 'Claude 1/21/2026 12:00'!
startUp: resuming
	"Called on image startup - check for --mcp flag"
	| args |
	resuming ifFalse: [ ^ self ].
	args := Smalltalk arguments.
	(args includes: '--mcp') ifTrue: [
		self startServer ].! !

!MCPServer class methodsFor: 'instance creation' stamp: 'Claude 1/21/2026 12:00'!
startServer
	"Start the MCP server in a background process.
	Redirects changes file to /dev/null to allow multiple MCP instances
	without conflicting writes to the same .changes file."

	"Disable changes file to avoid conflicts with multiple MCP sessions"
	SourceFiles at: 2 put: (FileStream fileNamed: '/dev/null').

	[
		"Brief delay to let system stabilize"
		(Delay forMilliseconds: 100) wait.
		self new run
	] forkAt: Processor userBackgroundPriority named: 'MCP Server'.! !

"After filing in, run:
  Installer squeakMap install: 'OSProcess'.
  Smalltalk addToStartUpList: MCPServer.
  Smalltalk saveImageAs: 'ClaudeMCP-Squeak'.
"!
