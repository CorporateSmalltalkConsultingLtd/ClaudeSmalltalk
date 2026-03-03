'From Cuis7.1 [latest update: #6610] on 20 January 2026 at 12:00:00 pm'!
'Description: Fix for clipboard copy when focus leaves text pane.

When the mouse/cursor moves outside an InnerTextMorph and focus changes,
the selection state should be preserved. This patch stores the selection
in textComposition when focus leaves the text morph.

Note: This preserves the VISUAL selection. If Cmd+C still does not work
after focus leaves, the issue may be that keyboard shortcuts are being
dispatched to the wrong morph. Further investigation would be needed.

Load with:
  CodePackageFile installPackage: ''/path/to/SelectionFocusFix.pck.st'' asFileEntry.

To test:
  1. Open a Browser (Cmd+B or World menu > Open > Browser)
  2. Navigate to any class and method
  3. Select some text in the code pane by clicking and dragging
  4. While keeping the selection, click on a method in the method list
     (this moves keyboard focus away from the code pane)
  5. Observe: the selection should still be visible in the code pane
  6. Press Cmd+C to copy
  7. Open a Workspace and Cmd+V to paste
  8. Check if the selected text was copied

If the selection disappears visually after step 4, the fix is not working.
If the selection stays visible but Cmd+C does not copy it, then the issue
is with keyboard shortcut routing (different problem).

To verify fix is loaded:
  (InnerTextMorph >> #keyboardFocusChange:) getSource includesSubString: ''FIX''
'!
!provides: 'SelectionFocusFix' 1 1!
SystemOrganization addCategory: #SelectionFocusFix!


!classDefinition: #SelectionFocusFixInfo category: #SelectionFocusFix!
Object subclass: #SelectionFocusFixInfo
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SelectionFocusFix'!

!SelectionFocusFixInfo class methodsFor: 'documentation' stamp: 'ClaudeMCP 1/20/2026 12:00'!
about
	"Information about this fix"

	^'SelectionFocusFix v1.1

This package modifies InnerTextMorph>>keyboardFocusChange: to store
the selection in textComposition when keyboard focus leaves the morph.

The goal is to preserve text selection when the user clicks outside
the text pane, so that copy operations can still access the selected text.

To check if installed:
  SelectionFocusFixInfo isInstalled

To see the modified method:
  (InnerTextMorph >> #keyboardFocusChange:) browse'! !

!SelectionFocusFixInfo class methodsFor: 'testing' stamp: 'ClaudeMCP 1/20/2026 12:00'!
isInstalled
	"Check if the fix is installed"

	^(InnerTextMorph >> #keyboardFocusChange:) getSource includesSubString: 'FIX'! !

!InnerTextMorph methodsFor: 'events' stamp: 'ClaudeMCP 1/20/2026 12:00'!
keyboardFocusChange: aBoolean
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw border feedback.

	FIX: When losing focus, store the selection in textComposition so it is preserved
	for potential copy operations even after focus has moved elsewhere."

	aBoolean
		ifTrue: [
			"A hand is wanting to send us characters..."
			editor ifNil: [ self editor storeSelectionInComposition ].	"Forces install"
			(self disablesEditing not and: [ editor hasSelection not ]) ifTrue: [
				self startBlinking ]]
		ifFalse: [
			"FIX: Store selection before losing focus so it can be recovered"
			editor ifNotNil: [ editor storeSelectionInComposition ].
			self stopBlinking ].

	(owner notNil and: [ owner drawsKeyboardFocusIndicator ])
		ifTrue: [ owner redrawNeeded ]
		ifFalse: [
			"Selection might be shown differently when focused"
			self textComposition selectionRects do: [ :r | self invalidateLocalRect: r ]].! !
