TestCase subclass: #SetWithNilTest
  instanceVariableNames: 
    ''
  classVariableNames: ''
  poolDictionaries: '' !


!SetWithNilTest methods !

testAddNil 

	"Verify SetWithNil can store and find nil."

    | s |
    s := SetWithNil new.
    s add: nil.
    self assert: (s includes: nil)!

testAddNilOnce 

	"Verify adding nil twice results in size 1."

    | s |
    s := SetWithNil new.
    s add: nil.
    s add: nil.
    self assert: s size = 1!

testAsSetElement 

	"Verify asSetElement protocol: non-nil returns self, nil returns SetElement."

    self assert: 42 asSetElement = 42.
    self assert: nil asSetElement class = SetElement.
    self assert: nil asSetElement enclosedElement isNil!

testDoEnumeratesNil 

	"Verify do: correctly enumerates nil elements."

    | s r |
    s := SetWithNil new.
    s add: nil.
    s add: 1.
    s add: 2.
    r := OrderedCollection new.
    s do: [:e | r add: e].
    self assert: r size = 3.
    self assert: (r includes: nil)!

testEmptySetWithNil 

	"Verify empty SetWithNil behaves correctly."

    | s |
    s := SetWithNil new.
    self assert: s size = 0.
    self deny: (s includes: nil).
    self assert: (s occurrencesOf: nil) = 0!

testMixedElements 

	"Verify SetWithNil works with nil and non-nil elements together."

    | s |
    s := SetWithNil new.
    s add: nil.
    s add: 42.
    s add: 'hello'.
    self assert: s size = 3.
    self assert: (s includes: nil).
    self assert: (s includes: 42).
    self assert: (s includes: 'hello').
    self deny: (s includes: 99)!

testNonNilPassthrough 

	"Verify non-nil elements work normally (no regressions)."

    | s |
    s := SetWithNil new.
    s add: 1.
    s add: 2.
    s add: 3.
    self assert: s size = 3.
    self assert: (s includes: 2).
    s remove: 2 ifAbsent: [self signalFailure: '2 not found'].
    self assert: s size = 2.
    self deny: (s includes: 2)!

testOccurrencesOfNil 

	"Verify occurrencesOf: works for nil."

    | s |
    s := SetWithNil new.
    s add: nil.
    self assert: (s occurrencesOf: nil) = 1.
    self assert: (s occurrencesOf: 99) = 0!

testRemoveNil 

	"Verify nil can be removed from SetWithNil."

    | s |
    s := SetWithNil new.
    s add: nil.
    s add: 42.
    s remove: nil ifAbsent: [self signalFailure: 'nil not found'].
    self assert: s size = 1.
    self deny: (s includes: nil).
    self assert: (s includes: 42)!

testSetCannotStoreNil 

	"Confirm standard Set cannot store nil (documents the bug SetWithNil fixes)."

    | s |
    s := Set new.
    s add: nil.
    self deny: (s includes: nil)!

testSetElementInSetWithNil 

	"Verify that adding a SetElement instance to a SetWithNil stores and retrieves it correctly.
	Without SetElement>>asSetElement, enclosedElement would unwrap it to wrappedObject."

    | s elem result |
    elem := SetElement new wrappedObject: 42.
    s := SetWithNil new.
    s add: elem.
    self assert: s size = 1.
    self assert: (s includes: elem).
    self deny: (s includes: 42).
    result := OrderedCollection new.
    s do: [:e | result add: e].
    self assert: result size = 1.
    self assert: result first class = SetElement.
    self assert: result first wrappedObject = 42.
    s remove: elem ifAbsent: [self signalFailure: 'SetElement not found'].
    self assert: s size = 0! !
