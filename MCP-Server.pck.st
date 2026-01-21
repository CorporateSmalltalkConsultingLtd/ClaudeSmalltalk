'From Cuis7.7 [latest update: #7777] on 20 January 2026 at 5:17:14 pm'!
'Description MCP Server for Claude Code integration.

Implements the Model Context Protocol (MCP) over stdio with JSON-RPC 2.0.
Uses JSON Lines protocol (one JSON message per line, no Content-Length headers).
Provides 12 tools for Smalltalk interaction (saveImage intentionally excluded).

Usage:
  1. Load this package into a Cuis image
  2. Save the image
  3. Run with --mcp flag:
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
!provides: 'MCP-Server' 1 4!
SystemOrganization addCategory: #'MCP-Server'!


!classDefinition: #MCPServer category: #'MCP-Server'!
Object subclass: #MCPServer
	instanceVariableNames: 'transport running'
	classVariableNames: ''
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


!MCPServer methodsFor: 'initialization' stamp: ''!
initialize
	transport := MCPTransport new.
	running := false.! !

!MCPServer methodsFor: 'running' stamp: ''!
run
	| temp1 temp2 |
	running := true.
	[ running ] whileTrue: [
		temp1 := nil.
		temp2 := nil.
		temp1 := transport readMessage.
		temp1
			ifNil: [ running := false ]
			ifNotNil: [
				temp2 := self handleRequest: temp1.
				temp2 ifNotNil: [ transport writeMessage: (Json render: temp2) ]]].! !

!MCPServer methodsFor: 'running' stamp: ''!
stop
	running := false.! !

!MCPServer methodsFor: 'request handling' stamp: ''!
dispatch: arg1 params: arg2
	arg1 = 'initialize' ifTrue: [ ^ self handleInitialize: arg2 ].
	arg1 = 'tools/list' ifTrue: [ ^ self handleToolsList: arg2 ].
	arg1 = 'tools/call' ifTrue: [ ^ self handleToolsCall: arg2 ].
	arg1 = 'ping' ifTrue: [ ^ Dictionary new ].
	self error: 'Unknown method: ' , arg1.! !

!MCPServer methodsFor: 'request handling' stamp: ''!
handleNotification: arg1 params: arg2
	arg1 = 'notifications/initialized' ifTrue: [ ^ self ].
	arg1 = 'notifications/cancelled' ifTrue: [ ^ self ].! !

!MCPServer methodsFor: 'request handling' stamp: ''!
handleRequest: arg1
	| temp2 temp3 temp4 |
	temp2 := arg1
		at: 'method'
		ifAbsent: [ ^ nil ].
	temp3 := arg1
		at: 'id'
		ifAbsent: [ ].
	temp3 ifNil: [
		self
			handleNotification: temp2
			params:
				(arg1
					at: 'params'
					ifAbsent: [ Dictionary new ]).
		^ nil ].
	[
	temp4 := self
		dispatch: temp2
		params:
			(arg1
				at: 'params'
				ifAbsent: [ Dictionary new ]).
	^ self
		successResponse: temp3
		result: temp4 ]
		on: Error
		do: [ :argm3_6 |
			^ self
				errorResponse: temp3
				code: -32603
				message: argm3_6 printString ].! !

!MCPServer methodsFor: 'responses' stamp: ''!
errorResponse: arg1 code: arg2 message: arg3
	^ OrderedDictionary new
		
			at: 'jsonrpc'
			put: '2.0';
		
			at: 'id'
			put: arg1;
		
			at: 'error'
			put:
				(OrderedDictionary new
					
						at: 'code'
						put: arg2;
					
						at: 'message'
						put: arg3;
					yourself);
		yourself.! !

!MCPServer methodsFor: 'responses' stamp: ''!
successResponse: arg1 result: arg2
	^ OrderedDictionary new
		
			at: 'jsonrpc'
			put: '2.0';
		
			at: 'id'
			put: arg1;
		
			at: 'result'
			put: arg2;
		yourself.! !

!MCPServer methodsFor: 'MCP handlers' stamp: ''!
handleInitialize: arg1
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
handleToolsCall: arg1
	| temp2 temp3 temp4 |
	temp2 := arg1 at: 'name'.
	temp3 := arg1
		at: 'arguments'
		ifAbsent: [ Dictionary new ].
	temp4 := self
		executeTool: temp2
		arguments: temp3.
	^ OrderedDictionary new
		
			at: 'content'
			put:
				{
					OrderedDictionary new
						
							at: 'type'
							put: 'text';
						
							at: 'text'
							put: temp4;
						yourself
				};
		yourself.! !

!MCPServer methodsFor: 'MCP handlers' stamp: ''!
handleToolsList: arg1
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
executeTool: arg1 arguments: arg2
	arg1 = 'smalltalk_evaluate' ifTrue: [ ^ self toolEvaluate: arg2 ].
	arg1 = 'smalltalk_browse' ifTrue: [ ^ self toolBrowse: arg2 ].
	arg1 = 'smalltalk_method_source' ifTrue: [ ^ self toolMethodSource: arg2 ].
	arg1 = 'smalltalk_define_class' ifTrue: [ ^ self toolDefineClass: arg2 ].
	arg1 = 'smalltalk_define_method' ifTrue: [ ^ self toolDefineMethod: arg2 ].
	arg1 = 'smalltalk_delete_method' ifTrue: [ ^ self toolDeleteMethod: arg2 ].
	arg1 = 'smalltalk_delete_class' ifTrue: [ ^ self toolDeleteClass: arg2 ].
	arg1 = 'smalltalk_list_classes' ifTrue: [ ^ self toolListClasses: arg2 ].
	arg1 = 'smalltalk_hierarchy' ifTrue: [ ^ self toolHierarchy: arg2 ].
	arg1 = 'smalltalk_subclasses' ifTrue: [ ^ self toolSubclasses: arg2 ].
	arg1 = 'smalltalk_list_categories' ifTrue: [ ^ self toolListCategories: arg2 ].
	arg1 = 'smalltalk_classes_in_category' ifTrue: [ ^ self toolClassesInCategory: arg2 ].
	arg1 = 'smalltalk_save_image' ifTrue: [ ^ self toolSaveImage: arg2 ].
	self error: 'Unknown tool: ' , arg1.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolBrowse: arg1
	| temp2 temp3 temp4 |
	temp2 := arg1
		at: 'className'
		ifAbsent: [ '' ].
	temp2 isEmpty ifTrue: [ self error: 'No className provided' ].
	temp3 := Smalltalk
		at: temp2 asSymbol
		ifAbsent: [ self error: 'Class not found: ' , temp2 ].
	temp4 := OrderedDictionary new.
	temp4
		at: 'name'
		put: temp3 name.
	temp4
		at: 'superclass'
		put:
			(temp3 superclass
				ifNil: [ 'nil' ]
				ifNotNil: [ :temp5 |
					temp5 name ]).
	temp4
		at: 'category'
		put: temp3 category.
	temp4
		at: 'instanceVariables'
		put: temp3 instVarNames asArray.
	temp4
		at: 'classVariables'
		put: temp3 classVarNames asArray.
	temp4
		at: 'methods'
		put: temp3 selectors asArray sorted.
	temp4
		at: 'comment'
		put: (temp3 comment ifNil: [ '' ]).
	^ Json render: temp4.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolClassesInCategory: arg1
	| temp2 temp3 temp4 |
	temp2 := arg1
		at: 'category'
		ifAbsent: [ '' ].
	temp2 isEmpty ifTrue: [ self error: 'No category provided' ].
	temp3 := Smalltalk allClasses select: [ :argm2_5 |
		argm2_5 category = temp2 asSymbol ].
	temp4 := (temp3 collect: [ :argm3_5 |
		argm3_5 name ]) asArray sorted.
	^ Json render: temp4.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolDefineClass: arg1
	| temp2 |
	temp2 := arg1
		at: 'definition'
		ifAbsent: [ '' ].
	temp2 isEmpty ifTrue: [ self error: 'No definition provided' ].
	Compiler evaluate: temp2.
	^ 'Class defined successfully'.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolDefineMethod: arg1
	| temp2 temp3 temp4 temp5 |
	temp2 := arg1
		at: 'className'
		ifAbsent: [ '' ].
	temp3 := arg1
		at: 'source'
		ifAbsent: [ '' ].
	temp4 := arg1
		at: 'category'
		ifAbsent: [ 'as yet unclassified' ].
	temp2 isEmpty ifTrue: [ self error: 'No className provided' ].
	temp3 isEmpty ifTrue: [ self error: 'No source provided' ].
	temp5 := Smalltalk
		at: temp2 asSymbol
		ifAbsent: [ self error: 'Class not found: ' , temp2 ].
	temp5
		compile: temp3
		classified: temp4.
	^ 'Method defined successfully'.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolDeleteClass: arg1
	| temp2 temp3 |
	temp2 := arg1
		at: 'className'
		ifAbsent: [ '' ].
	temp2 isEmpty ifTrue: [ self error: 'No className provided' ].
	temp3 := Smalltalk
		at: temp2 asSymbol
		ifAbsent: [ self error: 'Class not found: ' , temp2 ].
	temp3 removeFromSystem.
	^ 'Class deleted successfully'.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolDeleteMethod: arg1
	| temp2 temp3 temp4 |
	temp2 := arg1
		at: 'className'
		ifAbsent: [ '' ].
	temp3 := arg1
		at: 'selector'
		ifAbsent: [ '' ].
	temp2 isEmpty ifTrue: [ self error: 'No className provided' ].
	temp3 isEmpty ifTrue: [ self error: 'No selector provided' ].
	temp4 := Smalltalk
		at: temp2 asSymbol
		ifAbsent: [ self error: 'Class not found: ' , temp2 ].
	(temp4 includesSelector: temp3 asSymbol) ifFalse: [ self error: 'Method not found: ' , temp2 , '>>' , temp3 ].
	temp4 removeSelector: temp3 asSymbol.
	^ 'Method deleted successfully'.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolEvaluate: arg1
	| temp2 temp3 |
	temp2 := arg1
		at: 'code'
		ifAbsent: [ '' ].
	temp2 isEmpty ifTrue: [ self error: 'No code provided' ].
	temp3 := Compiler evaluate: temp2.
	^ temp3 printString.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolHierarchy: arg1
	| temp2 temp3 temp4 |
	temp2 := arg1
		at: 'className'
		ifAbsent: [ '' ].
	temp2 isEmpty ifTrue: [ self error: 'No className provided' ].
	temp3 := Smalltalk
		at: temp2 asSymbol
		ifAbsent: [ self error: 'Class not found: ' , temp2 ].
	temp4 := OrderedCollection new.
	[ temp3 notNil ] whileTrue: [
		temp4 add: temp3 name.
		temp3 := temp3 superclass ].
	^ Json render: temp4 asArray.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolListCategories: arg1
	| temp2 |
	temp2 := SystemOrganization categories asArray sorted.
	^ Json render: temp2.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolListClasses: arg1
	| temp2 temp3 temp4 |
	temp2 := arg1
		at: 'prefix'
		ifAbsent: [ '' ].
	temp3 := Smalltalk allClasses.
	temp2 isEmpty ifFalse: [
		temp3 := temp3 select: [ :argm2_5 |
			argm2_5 name asString beginsWith: temp2 ]].
	temp4 := (temp3 collect: [ :argm3_5 |
		argm3_5 name ]) asArray sorted.
	^ Json render: temp4.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolMethodSource: arg1
	| temp2 temp3 temp4 |
	temp2 := arg1
		at: 'className'
		ifAbsent: [ '' ].
	temp3 := arg1
		at: 'selector'
		ifAbsent: [ '' ].
	temp2 isEmpty ifTrue: [ self error: 'No className provided' ].
	temp3 isEmpty ifTrue: [ self error: 'No selector provided' ].
	temp4 := Smalltalk
		at: temp2 asSymbol
		ifAbsent: [ self error: 'Class not found: ' , temp2 ].
	(temp4 includesSelector: temp3 asSymbol) ifFalse: [ self error: 'Method not found: ' , temp2 , '>>' , temp3 ].
	^ (temp4 >> temp3 asSymbol) sourceCode.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolSaveImage: arg1
	Smalltalk saveImage.
	^ 'Image saved successfully'.! !

!MCPServer methodsFor: 'tool implementations' stamp: ''!
toolSubclasses: arg1
	| temp2 temp3 temp4 |
	temp2 := arg1
		at: 'className'
		ifAbsent: [ '' ].
	temp2 isEmpty ifTrue: [ self error: 'No className provided' ].
	temp3 := Smalltalk
		at: temp2 asSymbol
		ifAbsent: [ self error: 'Class not found: ' , temp2 ].
	temp4 := (temp3 subclasses collect: [ :argm3_5 |
		argm3_5 name ]) asArray sorted.
	^ Json render: temp4.! !

!MCPServer class methodsFor: 'system startup' stamp: ''!
startUp: arg1
	| temp2 |
	arg1 ifFalse: [ ^ self ].
	SourceFiles
		at: 2
		put: nil.
	temp2 := Smalltalk startUpArguments.
	(temp2 includes: '--mcp') ifTrue: [ self startServer ].! !

!MCPServer class methodsFor: 'instance creation' stamp: ''!
startServer
	(Delay forMilliseconds: 500) wait.
	self new run.! !

!MCPTransport methodsFor: 'initialization' stamp: ''!
initialize
	stdin := StdIOReadStream stdin.
	stdout := StdIOWriteStream stdout.! !

!MCPTransport methodsFor: 'reading' stamp: ''!
readLine
	| temp1 temp2 |
	temp1 := String new writeStream.
	[
	temp2 := stdin next.
	temp2 isNil ifTrue: [ ^ nil ].
	(temp2 = Character cr or: [ temp2 = Character lf ]) ifTrue: [ ^ temp1 contents ].
	temp1 nextPut: temp2 ] repeat.! !

!MCPTransport methodsFor: 'reading' stamp: ''!
readMessage
	| temp1 |
	temp1 := self readLine.
	temp1 isNil ifTrue: [ ^ nil ].
	temp1 isEmpty ifTrue: [ ^ self readMessage ].
	^ Json readFrom: temp1 readStream.! !

!MCPTransport methodsFor: 'writing' stamp: ''!
writeMessage: arg1
	| temp2 |
	temp2 := arg1 copyWithout: Character cr.
	temp2 := temp2 copyWithout: Character lf.
	stdout
		nextPutAll: temp2;
		newLine;
		flush.! !

!SystemDictionary methodsFor: '*MCP-Server' stamp: ''!
checkIfAlreadyRunningOrStoppedNoExit
	| temp1 temp3 |
	self currentChangesName ifNil: [ ^ self ].
	temp1 := nil.
	temp3 := nil.
	self withChangesFileDo: [ :argm3_4 |
		argm3_4 position: self lastQuitLogPosition.
		argm3_4 atEnd ifFalse: [ temp1 := argm3_4 nextChunk ].
		argm3_4 atEnd ifFalse: [ temp3 := argm3_4 nextChunk ]].
	(temp1 isNil or: [
		((Smalltalk isQuitRecord: temp1) or: [ Smalltalk isSnapshotRecord: temp1 ]) not ]) ifTrue: [
		UISupervisor whenUIinSafeState: [ self notify: self currentChangesName asFullFileEntry pathName , String newLineString , 'Image save chunk not found. Possible image / changes file corruption. Proceed with caution.' ].
		^ self ].
	temp3 notNil ifTrue: [
		(Preferences at: #checkLostChangesOnStartUp)
			ifTrue: [ UISupervisor whenUIinSafeState: [ self restoreLostChanges ]]
			ifFalse: [
				(Preferences at: #warnAndOfferLastUserChanges) ifTrue: [ UISupervisor whenUIinSafeState: [ self warnAndOfferLastUserChanges ]]]].! !

!SystemDictionary methodsFor: '*MCP-Server' stamp: ''!
openSourcesAndChanges
	| temp1 temp2 temp3 temp4 temp5 temp6 temp7 temp8 temp9 temp11 |
	temp6 := false.
	temp8 := 2.
	[
	temp8 <= 100 and: [ (temp7 := self getSystemAttribute: temp8) notNil ]] whileTrue: [
		(temp7 = '--mcp' or: [ temp7 = '-d' ]) ifTrue: [ temp6 := true ].
		temp8 := temp8 + 1 ].
	temp1 := SourceFiles at: 1.
	temp1 ifNil: [
		temp9 := self defaultSourcesName asFullFileEntry.
		temp9 exists ifFalse: [ temp9 := self alternativeSourcesName asFullFileEntry ].
		temp9 exists ifTrue: [
			temp1 := [ temp9 readStream ]
				on: FileDoesNotExistException
				do: [ ]]].
	(temp1 isNil and: [ Preferences at: #warnIfNoSourcesFile ]) ifTrue: [
		temp3 := 'Cuis cannot locate the sources file named ' , temp9 pathName , '.' , String newLineString , 'Please check that the file is properly named and is in the same directory as this image.'.
		self logStartupError: temp3 ].
	temp6 ifTrue: [
		SourceFiles := Array
			with: temp1
			with: nil.
		^ self ].
	temp2 := SourceFiles at: 2.
	temp2 ifNil: [
		temp9 := self defaultChangesName asFullFileEntry.
		temp5 := temp9 pathName.
		temp9 exists
			ifTrue: [
				temp4 := self lastQuitLogPosition.
				temp4 > 0 ifTrue: [
					temp9 readStreamDo: [ :argm11_12 |
						argm11_12 position: temp4.
						temp11 := argm11_12 nextChunk ].
					((temp11 beginsWith: self tagHeader) and: [ temp11 includesSubString: 'priorSource: ' ]) ifFalse: [
						(Preferences at: #warnIfNoChangesFile) ifTrue: [ self logStartupError: 'Incorrect changes file: ' , temp5 , String newLineString , 'Missing code will be decompiled' , String newLineString , 'New source code will not be saved' ].
						temp9 := nil ]]]
			ifFalse: [
				(Preferences at: #warnIfNoChangesFile) ifTrue: [ self logStartupError: 'Could not find changes file: ' , temp5 , String newLineString , 'Missing code will be decompiled' , String newLineString , 'New source code will not be saved' ].
				temp9 := nil ].
		temp9 ifNotNil: [
			temp2 := [ temp9 appendStream ]
				on: FileWriteError
				do: [ self logStartupError: 'Could not write to changes file: ' , temp5 , String newLineString , 'Changes file will not be used.' , String newLineString , 'Missing code will be decompiled' , String newLineString , 'New source code will not be saved' ]]].
	ChangesInitialFileSize := temp2 ifNotNil: [ temp2 position ].
	SourceFiles := Array
		with: temp1
		with: temp2.! !

"Register MCPServer for startup"!
Smalltalk addToStartUpList: MCPServer!
