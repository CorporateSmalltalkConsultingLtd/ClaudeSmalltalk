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
	"Define a new class"
	| definition |
	definition := args at: 'definition' ifAbsent: [ '' ].
	definition isEmpty ifTrue: [ self error: 'No definition provided' ].
	Compiler evaluate: definition.
	^ 'Class defined successfully'.! !

!MCPServer methodsFor: 'tool implementations' stamp: 'Claude 1/21/2026 12:00'!
toolDefineMethod: args
	"Define or modify a method on a class.
	 Uses compileSilently: to avoid UI dialogs in headless mode."
	| className source category class |
	className := args at: 'className' ifAbsent: [ '' ].
	source := args at: 'source' ifAbsent: [ '' ].
	category := args at: 'category' ifAbsent: [ 'as yet unclassified' ].
	className isEmpty ifTrue: [ self error: 'No className provided' ].
	source isEmpty ifTrue: [ self error: 'No source provided' ].
	class := Smalltalk at: className asSymbol ifAbsent: [
		self error: 'Class not found: ', className ].
	class compileSilently: source classified: category.
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
	"Try source file first, fall back to decompilation if .changes is unavailable
	 (e.g. playground mode where changes → /dev/null)"
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
	"Save the current image in place. Only works in dev mode.
	Uses headlessSave which skips Cursor/Morphic UI operations."
	| changesFile |
	changesFile := SourceFiles at: 2.
	(changesFile isNil or: [ changesFile name = '/dev/null' ])
		ifTrue: [ self error: 'Save is only available in dev mode. Start with --dev --image.' ].
	self class headlessSave.
	^ 'Image saved: ', Smalltalk imageName.! !

!MCPServer methodsFor: 'tool implementations' stamp: 'jmm 1/29/2026 21:00'!
toolSaveAsNewVersion: args
	"Save image/changes as next version number. Only works in dev mode.
	Uses headlessSaveAsNewVersion to avoid UI hangs under xvfb."
	| changesFile |
	changesFile := SourceFiles at: 2.
	(changesFile isNil or: [ changesFile name = '/dev/null' ])
		ifTrue: [ self error: 'Save is only available in dev mode. Start with --dev --image.' ].
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

!MCPServer class methodsFor: 'system startup' stamp: 'jmm 1/30/2026 03:45'!
startUp: resuming
	"Called on image startup. Two modes:
	 1. --mcp flag: Original Claude Code MCP mode (forked, background)
	 2. SMALLTALK_MCP_DAEMON=1 env var: Daemon mode (inline, blocking)
	 
	 Daemon mode runs during processStartUpList: which fires BEFORE
	 Project current wakeUpTopWindow — critical for headless operation
	 under xvfb-run where Morphic operations would block."
	| args |
	resuming ifFalse: [ ^ self ].
	
	"Daemon mode — set by smalltalk-daemon.py via env var"
	(self getenv: 'SMALLTALK_MCP_DAEMON') = '1' ifTrue: [
		^ self startDaemon ].
	
	"Original MCP mode — set by --mcp command line flag"
	args := Smalltalk arguments.
	(args includes: '--mcp') ifTrue: [
		self startServer ].! !

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
	   7 - JMM-509: Class-side method support in browse and method-source"
	^ 7! !

!MCPServer class methodsFor: 'instance creation' stamp: 'jmm 1/30/2026 03:45'!
startServer
	"Start the MCP server in a background process.
	In playground mode: Redirects changes file to /dev/null for safety.
	In dev mode (SMALLTALK_DEV_MODE=1): Keeps changes file for persistence."
	
	| devMode |
	
	"CRITICAL: Refresh OSProcess handles for new VM session (JMM-515).
	 After image save/restore, OSProcess session ID becomes stale,
	 causing stdin/stdout to report as closed. This refresh reattaches
	 the file handles to the current VM session."
	OSProcess thisOSProcess refreshFromProcessAccessor.
	
	devMode := (self getenv: 'SMALLTALK_DEV_MODE') = '1'.
	
	devMode 
		ifTrue: [
			"Dev mode - keep changes file for persistence"
			Transcript show: 'MCP Server starting in DEVELOPMENT mode (changes enabled)'; cr ]
		ifFalse: [
			"Playground mode - disable changes file for safety"
			SourceFiles at: 2 put: (FileStream fileNamed: '/dev/null').
			Transcript show: 'MCP Server starting in PLAYGROUND mode (changes disabled)'; cr ].

	[
		"Brief delay to let system stabilize"
		(Delay forMilliseconds: 100) wait.
		self new run
	] forkAt: Processor userBackgroundPriority named: 'MCP Server'.! !

!MCPServer class methodsFor: 'instance creation' stamp: 'jmm 1/30/2026 03:45'!
startDaemon
	"Start MCP server for daemon operation.
	Called from startUp: when SMALLTALK_MCP_DAEMON=1.
	Runs inline (blocking) at full priority to avoid green thread starvation.
	
	Environment variables:
	  SMALLTALK_MCP_DAEMON=1  - triggers this mode
	  SMALLTALK_DEV_MODE=1    - keeps .changes file (else /dev/null)
	  SMALLTALK_CHANGES_PATH  - explicit path for .changes file (dev mode)"
	
	| devMode changesPath |
	
	"Refresh OSProcess handles for new VM session (JMM-515)"
	OSProcess thisOSProcess refreshFromProcessAccessor.
	
	"Fix working directory to match image location (may differ cross-platform).
	 FileDirectory default uses the OS working directory, which may not match
	 the image location after cross-platform transfer."
	OSProcess thisOSProcess processAccessor chDir: (FileDirectory dirPathFor: Smalltalk imageName).
	
	devMode := (self getenv: 'SMALLTALK_DEV_MODE') = '1'.
	
	devMode 
		ifTrue: [
			changesPath := self getenv: 'SMALLTALK_CHANGES_PATH'.
			(changesPath notNil and: [ changesPath notEmpty ])
				ifTrue: [ SourceFiles at: 2 put: (FileStream oldFileOrNoneNamed: changesPath) ].
			Transcript show: 'MCP Daemon starting in DEVELOPMENT mode'; cr ]
		ifFalse: [
			SourceFiles at: 2 put: (FileStream fileNamed: '/dev/null').
			Transcript show: 'MCP Daemon starting in PLAYGROUND mode'; cr ].
	
	"Run inline — blocks this process, which is fine since we're in startUp:
	 and don't want Morphic/wakeUpTopWindow to run anyway"
	self new run.! !

!MCPServer class methodsFor: 'instance creation' stamp: 'jmm 1/30/2026 03:45'!
startServerInline
	"Start the MCP server inline (no fork).
	Used by the daemon's --doit to run the server at full priority,
	avoiding green thread starvation under xvfb-run.
	Performs the same setup as startServer but runs synchronously."
	
	| devMode |
	
	devMode := (self getenv: 'SMALLTALK_DEV_MODE') = '1'.
	
	devMode 
		ifTrue: [
			Transcript show: 'MCP Server starting INLINE in DEVELOPMENT mode (changes enabled)'; cr ]
		ifFalse: [
			SourceFiles at: 2 put: (FileStream fileNamed: '/dev/null').
			Transcript show: 'MCP Server starting INLINE in PLAYGROUND mode (changes disabled)'; cr ].

	self new run.! !

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

"After filing in, run:
  Installer squeakMap install: 'OSProcess'.
  Smalltalk addToStartUpList: MCPServer.
  Smalltalk saveImageAs: 'ClaudeMCP-Squeak'.
"!
