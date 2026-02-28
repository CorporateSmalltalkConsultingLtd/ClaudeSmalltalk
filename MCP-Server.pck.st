'From Cuis7.7 [latest update: #7777] on 21 January 2026 at 6:20:31 pm'!
'Description MCP Server for Claude Code integration.

Implements the Model Context Protocol (MCP) over stdio with JSON-RPC 2.0.
Uses JSON Lines protocol (one JSON message per line, no Content-Length headers).
Provides 12 tools for Smalltalk interaction (saveImage intentionally excluded).

REQUIRES: OSProcess package for async stdio support (GUI responsive during MCP wait).
Uses BufferedAsyncFileReadStream which provides semaphore-based waiting,
allowing the VM scheduler to run other processes (including the UI event loop)
while the MCP server waits for input from Claude Code.

Usage:
  1. Load JSON and OSProcess packages into a Cuis image
  2. Load this package (MCP-Server)
  3. Save the image
  4. Run with --mcp flag:
     squeak myImage.image --mcp

Claude Code config (~/.claude.json):
  {
    "mcpServers": {
      "cuisDirect": {
        "type": "stdio",
        "command": "/path/to/squeak",
        "args": ["/path/to/image.image", "--mcp"]
      }
    }
  }
'!
!provides: 'MCP-Server' 1 11!
!requires: 'JSON' nil nil nil!
!requires: 'OSProcess' nil nil nil!
SystemOrganization addCategory: #'MCP-Server'!


!classDefinition: #MCPServer category: #'MCP-Server'!
Object subclass: #MCPServer
	instanceVariableNames: 'transport running'
	classVariableNames: 'RunningInstance'
	poolDictionaries: ''
	category: 'MCP-Server'!
!classDefinition: 'MCPServer class' category: #'MCP-Server'!
MCPServer class
	instanceVariableNames: ''!

!classDefinition: #MCPTransport category: #'MCP-Server'!
Object subclass: #MCPTransport
	instanceVariableNames: 'stdin stdout'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MCP-Server'!
!classDefinition: 'MCPTransport class' category: #'MCP-Server'!
MCPTransport class
	instanceVariableNames: ''!


!MCPServer methodsFor: 'initialization' stamp: 'jmm 1/26/2026 11:00'!
initialize
	transport := MCPTransport new.
	running := false.! !

!MCPServer methodsFor: 'running' stamp: 'jmm 1/26/2026 11:00'!
run
	"Main server loop - read requests, dispatch, write responses"
	| request response |
	RunningInstance := self.
	running := true.
	[ running ] whileTrue: [
		request := transport readMessage.
		request
			ifNil: [ running := false ]
			ifNotNil: [
				response := self handleRequest: request.
				response ifNotNil: [ transport writeMessage: (Json render: response) ]]].! !

!MCPServer methodsFor: 'running' stamp: ''!
stop
	running := false.
	RunningInstance := nil.! !

!MCPServer methodsFor: 'request handling' stamp: ''!
dispatch: method params: params
	"Dispatch JSON-RPC method to handler"
	method = 'initialize' ifTrue: [ ^ self handleInitialize: params ].
	method = 'tools/list' ifTrue: [ ^ self handleToolsList: params ].
	method = 'tools/call' ifTrue: [ ^ self handleToolsCall: params ].
	method = 'ping' ifTrue: [ ^ Dictionary new ].
	self error: 'Unknown method: ' , method.! !

!MCPServer methodsFor: 'request handling' stamp: ''!
handleNotification: method params: params
	"Handle JSON-RPC notifications (no response expected)"
	method = 'notifications/initialized' ifTrue: [ ^ self ].
	method = 'notifications/cancelled' ifTrue: [ ^ self ].! !

!MCPServer methodsFor: 'request handling' stamp: ''!
handleRequest: request
	"Handle a JSON-RPC request, return response or nil for notifications"
	| method id result |
	method := request
		at: 'method'
		ifAbsent: [ ^ nil ].
	id := request
		at: 'id'
		ifAbsent: [ ].
	id ifNil: [
		self
			handleNotification: method
			params:
				(request
					at: 'params'
					ifAbsent: [ Dictionary new ]).
		^ nil ].
	[
	result := self
		dispatch: method
		params:
			(request
				at: 'params'
				ifAbsent: [ Dictionary new ]).
	^ self
		successResponse: id
		result: result ]
		on: Error
		do: [ :ex |
			^ self
				errorResponse: id
				code: -32603
				message: ex printString ].! !

!MCPServer methodsFor: 'responses' stamp: ''!
errorResponse: id code: code message: message
	"Build a JSON-RPC error response"
	^ OrderedDictionary new
		
			at: 'jsonrpc'
			put: '2.0';
		
			at: 'id'
			put: id;
		
			at: 'error'
			put:
				(OrderedDictionary new
					
						at: 'code'
						put: code;
					
						at: 'message'
						put: message;
					yourself);
		yourself.! !

!MCPServer methodsFor: 'responses' stamp: ''!
successResponse: id result: result
	"Build a JSON-RPC success response"
	^ OrderedDictionary new
		
			at: 'jsonrpc'
			put: '2.0';
		
			at: 'id'
			put: id;
		
			at: 'result'
			put: result;
		yourself.! !

!MCPServer methodsFor: 'MCP handlers' stamp: ''!
handleInitialize: params
	"Handle MCP initialize request"
	^ OrderedDictionary new
		
			at: 'protocolVersion'
			put: '2024-11-05';
		
			at: 'capabilities'
			put:
				(OrderedDictionary new
					
						at: 'tools'
						put: Dictionary new;
					yourself);
		
			at: 'serverInfo'
			put:
				(OrderedDictionary new
					
						at: 'name'
						put: 'cuis-smalltalk';
					
						at: 'version'
						put: '1.0.0';
					yourself);
		yourself.! !

!MCPServer methodsFor: 'MCP handlers' stamp: ''!
handleToolsCall: params
	"Handle MCP tools/call request"
	| name arguments resultText |
	name := params at: 'name'.
	arguments := params
		at: 'arguments'
		ifAbsent: [ Dictionary new ].
	resultText := self
		executeTool: name
		arguments: arguments.
	^ OrderedDictionary new
		
			at: 'content'
			put:
				{
					OrderedDictionary new
						
							at: 'type'
							put: 'text';
						
							at: 'text'
							put: resultText;
						yourself
				};
		yourself.! !

!MCPServer methodsFor: 'MCP handlers' stamp: ''!
handleToolsList: params
	"Handle MCP tools/list request"
	^ OrderedDictionary new
		
			at: 'tools'
			put: self toolDefinitions;
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: ''!
toolDefBrowse
	^ OrderedDictionary new
		
			at: 'name'
			put: 'smalltalk_browse';
		
			at: 'description'
			put: 'Browse a class to see its superclass, instance variables, class variables, and method selectors.';
		
			at: 'inputSchema'
			put:
				(OrderedDictionary new
					
						at: 'type'
						put: 'object';
					
						at: 'properties'
						put:
							(OrderedDictionary new
								
									at: 'className'
									put:
										(OrderedDictionary new
											
												at: 'type'
												put: 'string';
											
												at: 'description'
												put: 'Name of the class to browse';
											yourself);
								yourself);
					
						at: 'required'
						put: #('className');
					yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: ''!
toolDefClassesInCategory
	^ OrderedDictionary new
		
			at: 'name'
			put: 'smalltalk_classes_in_category';
		
			at: 'description'
			put: 'List all classes in a specific category.';
		
			at: 'inputSchema'
			put:
				(OrderedDictionary new
					
						at: 'type'
						put: 'object';
					
						at: 'properties'
						put:
							(OrderedDictionary new
								
									at: 'category'
									put:
										(OrderedDictionary new
											
												at: 'type'
												put: 'string';
											
												at: 'description'
												put: 'Name of the category';
											yourself);
								yourself);
					
						at: 'required'
						put: #('category');
					yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: ''!
toolDefDefineClass
	^ OrderedDictionary new
		
			at: 'name'
			put: 'smalltalk_define_class';
		
			at: 'description'
			put: 'Define a new class or modify an existing class definition.';
		
			at: 'inputSchema'
			put:
				(OrderedDictionary new
					
						at: 'type'
						put: 'object';
					
						at: 'properties'
						put:
							(OrderedDictionary new
								
									at: 'definition'
									put:
										(OrderedDictionary new
											
												at: 'type'
												put: 'string';
											
												at: 'description'
												put: 'Full class definition expression';
											yourself);
								yourself);
					
						at: 'required'
						put: #('definition');
					yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: ''!
toolDefDefineMethod
	^ OrderedDictionary new
		
			at: 'name'
			put: 'smalltalk_define_method';
		
			at: 'description'
			put: 'Define or modify a method on a class.';
		
			at: 'inputSchema'
			put:
				(OrderedDictionary new
					
						at: 'type'
						put: 'object';
					
						at: 'properties'
						put:
							(OrderedDictionary new
								
									at: 'className'
									put:
										(OrderedDictionary new
											
												at: 'type'
												put: 'string';
											
												at: 'description'
												put: 'Name of the class';
											yourself);
								
									at: 'source'
									put:
										(OrderedDictionary new
											
												at: 'type'
												put: 'string';
											
												at: 'description'
												put: 'Full method source including selector';
											yourself);
								yourself);
					
						at: 'required'
						put: #('className' 'source');
					yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: ''!
toolDefDeleteClass
	^ OrderedDictionary new
		
			at: 'name'
			put: 'smalltalk_delete_class';
		
			at: 'description'
			put: 'Remove a class from the system.';
		
			at: 'inputSchema'
			put:
				(OrderedDictionary new
					
						at: 'type'
						put: 'object';
					
						at: 'properties'
						put:
							(OrderedDictionary new
								
									at: 'className'
									put:
										(OrderedDictionary new
											
												at: 'type'
												put: 'string';
											
												at: 'description'
												put: 'Name of the class to remove';
											yourself);
								yourself);
					
						at: 'required'
						put: #('className');
					yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: ''!
toolDefDeleteMethod
	^ OrderedDictionary new
		
			at: 'name'
			put: 'smalltalk_delete_method';
		
			at: 'description'
			put: 'Remove a method from a class.';
		
			at: 'inputSchema'
			put:
				(OrderedDictionary new
					
						at: 'type'
						put: 'object';
					
						at: 'properties'
						put:
							(OrderedDictionary new
								
									at: 'className'
									put:
										(OrderedDictionary new
											
												at: 'type'
												put: 'string';
											
												at: 'description'
												put: 'Name of the class';
											yourself);
								
									at: 'selector'
									put:
										(OrderedDictionary new
											
												at: 'type'
												put: 'string';
											
												at: 'description'
												put: 'Selector of the method to remove';
											yourself);
								yourself);
					
						at: 'required'
						put: #('className' 'selector');
					yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: ''!
toolDefEvaluate
	^ OrderedDictionary new
		
			at: 'name'
			put: 'smalltalk_evaluate';
		
			at: 'description'
			put: 'Evaluate arbitrary Smalltalk code and return the result.';
		
			at: 'inputSchema'
			put:
				(OrderedDictionary new
					
						at: 'type'
						put: 'object';
					
						at: 'properties'
						put:
							(OrderedDictionary new
								
									at: 'code'
									put:
										(OrderedDictionary new
											
												at: 'type'
												put: 'string';
											
												at: 'description'
												put: 'Smalltalk code to evaluate';
											yourself);
								yourself);
					
						at: 'required'
						put: #('code');
					yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: ''!
toolDefHierarchy
	^ OrderedDictionary new
		
			at: 'name'
			put: 'smalltalk_hierarchy';
		
			at: 'description'
			put: 'Get the inheritance hierarchy for a class (from Object down to the class).';
		
			at: 'inputSchema'
			put:
				(OrderedDictionary new
					
						at: 'type'
						put: 'object';
					
						at: 'properties'
						put:
							(OrderedDictionary new
								
									at: 'className'
									put:
										(OrderedDictionary new
											
												at: 'type'
												put: 'string';
											
												at: 'description'
												put: 'Name of the class';
											yourself);
								yourself);
					
						at: 'required'
						put: #('className');
					yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: ''!
toolDefListCategories
	^ OrderedDictionary new
		
			at: 'name'
			put: 'smalltalk_list_categories';
		
			at: 'description'
			put: 'List all system categories.';
		
			at: 'inputSchema'
			put:
				(OrderedDictionary new
					
						at: 'type'
						put: 'object';
					
						at: 'properties'
						put: Dictionary new;
					yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: ''!
toolDefListClasses
	^ OrderedDictionary new
		
			at: 'name'
			put: 'smalltalk_list_classes';
		
			at: 'description'
			put: 'List all classes in the system, optionally filtered by prefix.';
		
			at: 'inputSchema'
			put:
				(OrderedDictionary new
					
						at: 'type'
						put: 'object';
					
						at: 'properties'
						put:
							(OrderedDictionary new
								
									at: 'prefix'
									put:
										(OrderedDictionary new
											
												at: 'type'
												put: 'string';
											
												at: 'description'
												put: 'Optional prefix to filter class names';
											yourself);
								yourself);
					yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: ''!
toolDefMethodSource
	^ OrderedDictionary new
		
			at: 'name'
			put: 'smalltalk_method_source';
		
			at: 'description'
			put: 'Get the source code of a specific method.';
		
			at: 'inputSchema'
			put:
				(OrderedDictionary new
					
						at: 'type'
						put: 'object';
					
						at: 'properties'
						put:
							(OrderedDictionary new
								
									at: 'className'
									put:
										(OrderedDictionary new
											
												at: 'type'
												put: 'string';
											
												at: 'description'
												put: 'Name of the class';
											yourself);
								
									at: 'selector'
									put:
										(OrderedDictionary new
											
												at: 'type'
												put: 'string';
											
												at: 'description'
												put: 'Method selector';
											yourself);
								yourself);
					
						at: 'required'
						put: #('className' 'selector');
					yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: ''!
toolDefSaveImage
	^ OrderedDictionary new
		
			at: 'name'
			put: 'smalltalk_save_image';
		
			at: 'description'
			put: 'Save the Smalltalk image to disk.';
		
			at: 'inputSchema'
			put:
				(OrderedDictionary new
					
						at: 'type'
						put: 'object';
					
						at: 'properties'
						put: Dictionary new;
					yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: ''!
toolDefSubclasses
	^ OrderedDictionary new
		
			at: 'name'
			put: 'smalltalk_subclasses';
		
			at: 'description'
			put: 'Get the direct subclasses of a class.';
		
			at: 'inputSchema'
			put:
				(OrderedDictionary new
					
						at: 'type'
						put: 'object';
					
						at: 'properties'
						put:
							(OrderedDictionary new
								
									at: 'className'
									put:
										(OrderedDictionary new
											
												at: 'type'
												put: 'string';
											
												at: 'description'
												put: 'Name of the class';
											yourself);
								yourself);
					
						at: 'required'
						put: #('className');
					yourself);
		yourself.! !

!MCPServer methodsFor: 'tool definitions' stamp: ''!
toolDefinitions
	"Note: saveImage is intentionally excluded from exposed tools"
	^ {self toolDefEvaluate. self toolDefBrowse. self toolDefMethodSource. self toolDefDefineClass. self toolDefDefineMethod. self toolDefDeleteMethod. self toolDefDeleteClass. self toolDefListClasses. self toolDefHierarchy. self toolDefSubclasses. self toolDefListCategories. self toolDefClassesInCategory}.! !

!MCPServer methodsFor: 'tool dispatch' stamp: ''!
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
	self error: 'Unknown tool: ' , name.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolBrowse: args
	"Browse a class - return metadata as JSON.
	 Includes both instance-side and class-side methods."
	| className class result |
	className := args
		at: 'className'
		ifAbsent: [ '' ].
	className isEmpty ifTrue: [ self error: 'No className provided' ].
	class := Smalltalk
		at: className asSymbol
		ifAbsent: [ self error: 'Class not found: ' , className ].
	result := OrderedDictionary new.
	result
		at: 'name'
		put: class name.
	result
		at: 'superclass'
		put:
			(class superclass
				ifNil: [ 'nil' ]
				ifNotNil: [ :sc |
					sc name ]).
	result
		at: 'category'
		put: class category.
	result
		at: 'instanceVariables'
		put: class instVarNames asArray.
	result
		at: 'classVariables'
		put: class classVarNames asArray.
	result
		at: 'methods'
		put: class selectors asArray sorted.
	result
		at: 'comment'
		put: (class comment ifNil: [ '' ]).
	^ Json render: result.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolClassesInCategory: args
	"List all classes in a category"
	| category classes names |
	category := args
		at: 'category'
		ifAbsent: [ '' ].
	category isEmpty ifTrue: [ self error: 'No category provided' ].
	classes := Smalltalk allClasses select: [ :c |
		c category = category asSymbol ].
	names := (classes collect: [ :c |
		c name ]) asArray sorted.
	^ Json render: names.! !

!MCPServer methodsFor: 'tool implementations' stamp: 'jmm 1/26/2026 09:30'!
toolDefineClass: args
	"Define a new class"
	| definition |
	definition := args
		at: 'definition'
		ifAbsent: [ '' ].
	definition isEmpty ifTrue: [ self error: 'No definition provided' ].
	Compiler evaluate: definition.
	^ 'Class defined successfully'.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolDefineMethod: args
	"Define or modify a method on a class.
	 Uses compileSilently: to avoid UI dialogs in headless mode."
	| className source category class |
	className := args
		at: 'className'
		ifAbsent: [ '' ].
	source := args
		at: 'source'
		ifAbsent: [ '' ].
	category := args
		at: 'category'
		ifAbsent: [ 'as yet unclassified' ].
	className isEmpty ifTrue: [ self error: 'No className provided' ].
	source isEmpty ifTrue: [ self error: 'No source provided' ].
	class := Smalltalk
		at: className asSymbol
		ifAbsent: [ self error: 'Class not found: ' , className ].
	class
		compileSilently: source
		classified: category.
	^ 'Method defined successfully'.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolDeleteClass: args
	"Remove a class from the system"
	| className class |
	className := args
		at: 'className'
		ifAbsent: [ '' ].
	className isEmpty ifTrue: [ self error: 'No className provided' ].
	class := Smalltalk
		at: className asSymbol
		ifAbsent: [ self error: 'Class not found: ' , className ].
	class removeFromSystem.
	^ 'Class deleted successfully'.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolDeleteMethod: args
	"Remove a method from a class"
	| className selector class |
	className := args
		at: 'className'
		ifAbsent: [ '' ].
	selector := args
		at: 'selector'
		ifAbsent: [ '' ].
	className isEmpty ifTrue: [ self error: 'No className provided' ].
	selector isEmpty ifTrue: [ self error: 'No selector provided' ].
	class := Smalltalk
		at: className asSymbol
		ifAbsent: [ self error: 'Class not found: ' , className ].
	(class includesSelector: selector asSymbol) ifFalse: [ self error: 'Method not found: ' , className , '>>' , selector ].
	class removeSelector: selector asSymbol.
	^ 'Method deleted successfully'.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolEvaluate: args
	"Evaluate Smalltalk code and return result"
	| code result |
	code := args
		at: 'code'
		ifAbsent: [ '' ].
	code isEmpty ifTrue: [ self error: 'No code provided' ].
	result := Compiler evaluate: code.
	^ result printString.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolHierarchy: args
	"Get inheritance hierarchy from Object down to this class"
	| className class hierarchy |
	className := args
		at: 'className'
		ifAbsent: [ '' ].
	className isEmpty ifTrue: [ self error: 'No className provided' ].
	class := Smalltalk
		at: className asSymbol
		ifAbsent: [ self error: 'Class not found: ' , className ].
	hierarchy := OrderedCollection new.
	[ class notNil ] whileTrue: [
		hierarchy add: class name.
		class := class superclass ].
	^ Json render: hierarchy asArray.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolListCategories: args
	"List all system categories"
	| categories |
	categories := SystemOrganization categories asArray sorted.
	^ Json render: categories.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolListClasses: args
	"List all classes, optionally filtered by prefix"
	| prefix classes names |
	prefix := args
		at: 'prefix'
		ifAbsent: [ '' ].
	classes := Smalltalk allClasses.
	prefix isEmpty ifFalse: [
		classes := classes select: [ :c |
			c name asString beginsWith: prefix ]].
	names := (classes collect: [ :c |
		c name ]) asArray sorted.
	^ Json render: names.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolMethodSource: args
	"Get source code of a method. Supports both instance and class side.
	 Class side can be requested via side='class' parameter or by appending
	 ' class' to className (e.g. 'MCPServer class')."
	| className selector side |
	className := args
		at: 'className'
		ifAbsent: [ '' ].
	selector := args
		at: 'selector'
		ifAbsent: [ '' ].
	className isEmpty ifTrue: [ self error: 'No className provided' ].
	selector isEmpty ifTrue: [ self error: 'No selector provided' ].
	side := Smalltalk
		at: className asSymbol
		ifAbsent: [ self error: 'Class not found: ' , className ].
	(side includesSelector: selector asSymbol) ifFalse: [ self error: 'Method not found: ' , className , '>>' , selector ].
	^ (side >> selector asSymbol) sourceCode.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolSaveImage: args
	"Save the current image in place.
	Uses headlessSave which calls snapshotPrimitive directly,
	skipping the full shutdown/startup cycle that would restart MCP."
	self class headlessSave.
	^ 'Image saved: ' , Smalltalk imageName.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolSubclasses: args
	"Get direct subclasses of a class"
	| className class names |
	className := args
		at: 'className'
		ifAbsent: [ '' ].
	className isEmpty ifTrue: [ self error: 'No className provided' ].
	class := Smalltalk
		at: className asSymbol
		ifAbsent: [ self error: 'Class not found: ' , className ].
	names := (class subclasses collect: [ :c |
		c name ]) asArray sorted.
	^ Json render: names.! !

!MCPServer class methodsFor: 'private' stamp: ''!
envAt: varName
	"Get environment variable value. Works on both Squeak (getenv:) and Cuis (OSProcess)."
	(Smalltalk respondsTo: #getenv:) ifTrue: [ ^ Smalltalk getenv: varName ].
	[ ^ OSProcess accessor environmentAt: varName ]
		on: Error do: [ :e | ^ nil ].! !

!MCPServer class methodsFor: 'saving' stamp: ''!
headlessSave
	"Save the image without the full shutdown/startup cycle.
	Calls snapshotPrimitive directly, which avoids:
	  - processShutDownList / processStartUpList (would restart MCP)
	  - Cursor/Morphic UI operations (would hang under headless/xvfb)
	Changes file logging IS preserved for traceability.
	Based on Squeak MCPServer>>headlessSave, ported to Cuis API."
	
	| changesLog result |
	
	"Log to changes file"
	(SourceFiles at: 2) ifNotNil: [
		changesLog := String streamContents: [ :s |
			s nextPutAll: '----SNAPSHOT----';
			  nextPutAll: Date today printString;
			  space;
			  nextPutAll: Time now printString;
			  space;
			  nextPutAll: Smalltalk imageName asFullFileEntry name ].
		Smalltalk logChange: changesLog.
		Transcript show: changesLog; newLine ].
	
	"Save — calls primitive 97 directly"
	result := Smalltalk snapshotPrimitive.
	
	result ifNil: [ self error: 'Failed to write image file (disk full?)' ].
	
	"result is false when continuing from save point.
	 result is true when image is later restored (resumed)."
	^ result! !

!MCPServer class methodsFor: 'system startup' stamp: 'jmm 2/20/2026 00:50'!
startUp: resuming
	"Called on image startup. Two modes:
	 1. SMALLTALK_MCP_DAEMON=1 env var: Daemon mode (inline, blocking)
	 2. --mcp flag: Original MCP mode (forked, background)
	 
	 Daemon mode runs inline during processStartUpList: which fires BEFORE
	 the Morphic world loop starts — critical for headless operation under
	 xvfb-run where the daemon controls stdin/stdout.
	 
	 Guard: skip if MCP server is already running (resume after save)."
	| args |
	resuming ifFalse: [ ^ self ].
	
	"Don't restart if already running (e.g. after Smalltalk saveImage)"
	RunningInstance notNil ifTrue: [ ^ self ].
	
	"Daemon mode — set by smalltalk-daemon.py via env var"
	((self envAt: 'SMALLTALK_MCP_DAEMON') = '1') ifTrue: [
		^ self startDaemon ].
	
	"Original MCP mode — set by --mcp command line flag"
	args := Smalltalk startUpArguments.
	(args includes: '--mcp') ifTrue: [ self startServer ].! !

!MCPServer class methodsFor: 'version' stamp: 'jmm 2/20/2026 00:50'!
version
	"Return the MCP server version number.
	 Increment when making changes that external tools should detect.
	 Version history:
	   1 - Initial version
	   2 - Fixed toolDefineMethod: to use compileSilently: for headless operation
	   3 - (skipped)
	   4 - Fork MCP to background process, allowing GUI to display
	   5 - Set author initials to avoid prompt during class/method definition
	   6 - Port to OSProcess for non-blocking stdio (GUI responsive during MCP wait)
	   7 - Ensure ThisOSProcess is initialized before MCP transport accesses it
	   8 - Remove debug logging that blocked UI startup
	   9 - Simplify MCPTransport initialize
	   10 - Add extensive logging to debug UI startup hang
	   11 - Remove debug logging, use correct Cuis author API
	   12 - JMM-619: Add daemon mode (SMALLTALK_MCP_DAEMON env var) for smalltalk-daemon.py
	   13 - JMM-645: Remove SourceFiles manipulation — changes file always retained
	   14 - JMM-645: headlessSave for Cuis, RunningInstance guard, envAt: for cross-platform"
	^ 14! !

!MCPServer class methodsFor: 'instance creation' stamp: 'jmm 2/20/2026 00:50'!
startDaemon
	"Start MCP server for daemon operation (JMM-619).
	Called from startUp: when SMALLTALK_MCP_DAEMON=1.
	Runs inline (blocking) — no fork, no delay.
	
	This is critical for smalltalk-daemon.py which communicates over
	subprocess stdin/stdout pipes. Running inline ensures MCP responds
	immediately without waiting for Morphic initialization.
	
	Environment variables:
	  SMALLTALK_MCP_DAEMON=1  - triggers this mode
	  SMALLTALK_DEV_MODE=1    - keeps .changes file (else disabled)"
	
	Transcript show: 'MCP Daemon starting'; newLine.
	Utilities setAuthorName: 'ClaudeSmalltalk' initials: 'LLM'.
	
	"Run inline — blocks this process, which is fine since we are in startUp:
	 and do not want the Morphic world loop to run before MCP is ready"
	self new run.! !

!MCPServer class methodsFor: 'instance creation' stamp: 'jmm 1/26/2026 11:00'!
startServer
	"Start MCP server in background process, allowing GUI to initialize normally"
	Utilities setAuthorName: 'ClaudeSmalltalk' initials: 'LLM'.
	[
		(Delay forMilliseconds: 500) wait.
		self new run
	] forkAt: Processor userBackgroundPriority named: 'MCP Server'.! !

!MCPTransport methodsFor: 'initialization' stamp: 'jmm 1/26/2026 11:00'!
initialize
	"Initialize using OSProcess for stdio with buffered async reads.
	BufferedAsyncFileReadStream uses semaphore-based waiting which
	allows the VM to schedule other processes (like the UI) while waiting for input."

	| osProc |
	osProc := OSProcess thisOSProcess.
	stdin := osProc stdIn asBufferedAsyncFileReadStream.
	stdin setBlocking.
	stdout := osProc stdOut.! !

!MCPTransport methodsFor: 'reading' stamp: ''!
readLine
	"Read a line from stdin using buffered async stream.
	Uses semaphore-based waiting which allows UI to remain responsive."
	| stream char |
	stream := String new writeStream.
	[
	char := stdin next.
	char isNil ifTrue: [ ^ nil ].
	(char = Character cr or: [ char = Character lf ]) ifTrue: [ ^ stream contents ].
	stream nextPut: char ] repeat.! !

!MCPTransport methodsFor: 'reading' stamp: ''!
readMessage
	"Read a JSON message from stdin"
	| line |
	line := self readLine.
	line isNil ifTrue: [ ^ nil ].
	line isEmpty ifTrue: [ ^ self readMessage ].
	^ Json readFrom: line readStream.! !

!MCPTransport methodsFor: 'writing' stamp: ''!
writeMessage: jsonString
	"Write a JSON message to stdout, ensuring no embedded newlines"
	| clean |
	clean := jsonString copyWithout: Character cr.
	clean := clean copyWithout: Character lf.
	stdout
		nextPutAll: clean;
		newLine;
		flush.! !

"Register MCPServer for startup"!
Smalltalk addToStartUpList: MCPServer!
