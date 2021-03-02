;+----------------------------------------------------------------------------+
;|  Title: FrequentZ        Function: Tunes any stringed instrument
;|  Source: MASM32          License: None - Just some credit if copied
;|  Platform: Win32         Version: <see .rc file>
;|  Author: Dave Jones
;+----------------------------------------------------------------------------+


;+----------------------------------------------------------------------------+
;|  Assembler Directives:
.586
.model flat,stdcall
option casemap:none
;|
;+----------------------------------------------------------------------------+

;+----------------------------------------------------------------------------+
;|  Function Prototypes:
WinMain                 proto   :DWORD,:DWORD,:DWORD,:DWORD
InitSelector		    proto	:DWORD,:DWORD,:DWORD
ParseTuning			    proto	:DWORD
CreateTunerButtons      proto   :DWORD,:DWORD
DestroyTunerButtons	    proto	:DWORD,:DWORD
EnumerateTunings        proto   :DWORD,:DWORD,:DWORD
GetTuningByIndex	    proto	:DWORD,:DWORD
PlayNote			    proto	:DWORD
CheckMultiInstance	    proto	:DWORD,:DWORD,:DWORD,:DWORD
GetAppTitle			    proto	:DWORD,:DWORD,:DWORD,:DWORD
;|
;+----------------------------------------------------------------------------+

;+----------------------------------------------------------------------------+
;|  Includes:
;|      Headers:
include windows.inc
include user32.inc
include kernel32.inc
include winmm.inc
include version.inc
include masm32.inc
include gdi32.inc
;|
;|      Libraries:
includelib user32.lib
includelib kernel32.lib
includelib winmm.lib
includelib version.lib
includelib masm32.lib
includelib gdi32.lib
;|
;+----------------------------------------------------------------------------+

.const
;+----------------------------------------------------------------------------+
;|  Constants:
DEBUG_TEXT_SIZE          EQU     255
IDI_APPICONLG            EQU     5000
IDI_APPICONSM            EQU     5001
IDB_BKGROUND             EQU     5002
;|
;|  Structs:
;|
;+----------------------------------------------------------------------------+

.data
;+----------------------------------------------------------------------------+
;|  Globals:
;|      Initialized:
ClassName               db  "FREQUENTZMAINWINDOWCLASS",0
AppName                 db  "Frequentz",0
ClassButton             db  "BUTTON",0
ClassStatic             db  "STATIC",0
ClassComboBox           db  "COMBOBOX",0
MutexName               db  "Local\FREQUENTZIsAlreadyRunning",0
strChkLoopNote          db  "Loop",0
strStcTuning            db  "Tuning:",0
AppTitle                db  128     dup(0)
strDebugText            db  255     dup(0)
dwAppVersion            dd  0
hMidiOut                dd  0
dwDeviceId              dd  0
strError                db  "Error",0
strErrMsgOpenMidiDevice db  "Couldn't open default midi device.",0
hMIDIout                dd  0
AGUITAR                 dd  0000018C0h      ;## Change patch number to 24(Acoustic Guitar)
include	                tuningdb.inc        ;## Tunings database
dwTunerButtonStyle      dd  WS_VISIBLE+WS_CHILD
;|
;+----------------------------------------------------------------------------+

.data?
;+----------------------------------------------------------------------------+
;|      Uninitialized:
hInstance    	        HINSTANCE       ?
CommandLine  	        LPSTR           ?
hThread			        HANDLE		    ?
hHeap                   HANDLE          ?
hMidi                   HANDLE          ?
hwndChkLoopNote         HANDLE          ?
hwndStcTuning           HANDLE          ?
hwndCbbTuningSelection  HANDLE          ?
pWindowHandles          DWORD	        ?
dwButtonCount			DWORD	        ?
strHttpHdrUserAgent     BYTE    255 dup(?)
hBkBrush                DWORD           ?
;|
;+----------------------------------------------------------------------------+


;+----------------------------------------------------------------------------+
.code   ;##: Start of main code :##
;+----------------------------------------------------------------------------+
;|  Startup:
start:
	invoke GetModuleHandle, NULL
	mov    hInstance,eax

	invoke GetCommandLine
	mov    CommandLine,eax

    ;## Grab a heap handle
	invoke	HeapCreate, NULL, 255, 2048
	mov		hHeap, eax

    ;##: Check if we're already running
    invoke  CheckMultiInstance, hInstance, ADDR AppName, ADDR ClassName, ADDR MutexName
    .IF(eax!=TRUE)
        xor     eax, eax
        invoke  ExitProcess, eax
    .ENDIF

	;## Entry point for all windows programs
	invoke WinMain, hInstance,NULL,CommandLine, SW_SHOWDEFAULT

	;## Free and destroy the heap
	invoke  HeapFree, hHeap, NULL, pWindowHandles
	invoke  HeapDestroy, hHeap

	invoke ExitProcess,eax

WinMain proc hInst:HINSTANCE,hPrevInst:HINSTANCE,CmdLine:LPSTR,CmdShow:DWORD
	LOCAL wc:WNDCLASSEX
	LOCAL msg:MSG
	LOCAL hwnd:HWND

	mov     wc.cbSize,SIZEOF WNDCLASSEX
	mov     wc.style, CS_HREDRAW or CS_VREDRAW
	mov     wc.lpfnWndProc, OFFSET WndProc
	mov     wc.cbClsExtra,NULL
	mov     wc.cbWndExtra,NULL
	push    hInstance
	pop     wc.hInstance
    invoke  LoadImage, hInst, IDB_BKGROUND, IMAGE_BITMAP, NULL, NULL, LR_DEFAULTCOLOR
    invoke  CreatePatternBrush, eax
    mov     hBkBrush, eax
	mov     wc.hbrBackground, eax
    invoke  LoadImage, hInst, IDI_APPICONLG, IMAGE_ICON, NULL, NULL, LR_DEFAULTCOLOR
    mov     wc.hIcon, eax
    invoke  LoadImage, hInst, IDI_APPICONSM, IMAGE_ICON, NULL, NULL, LR_DEFAULTCOLOR
    mov     wc.hIconSm, eax
	mov     wc.lpszMenuName,NULL
	mov     wc.lpszClassName,OFFSET ClassName
	invoke  LoadCursor,NULL,IDC_ARROW
	mov     wc.hCursor,eax

	invoke  RegisterClassEx, addr wc
    invoke  GetAppTitle, hInst, ADDR AppName, ADDR AppTitle, ADDR strHttpHdrUserAgent
    mov     dwAppVersion, eax
	INVOKE  CreateWindowEx,NULL,ADDR ClassName,ADDR AppTitle, WS_SYSMENU,200,200,400,300,NULL,NULL,hInst,NULL
	mov     hwnd,eax

	invoke  ShowWindow, hwnd,SW_SHOWNORMAL
	invoke  UpdateWindow, hwnd

	.WHILE (TRUE)
		invoke GetMessage, ADDR msg,NULL,0,0
		.BREAK .IF (!eax)
		invoke TranslateMessage, ADDR msg
		invoke DispatchMessage, ADDR msg
	.ENDW

	mov     eax,msg.wParam
	ret
WinMain endp

WndProc proc hWnd:HWND, uMsg:UINT, wParam:WPARAM, lParam:LPARAM
	LOCAL   hinst,i
	LOCAL   dwMidiMessage:DWORD

	.IF (uMsg==WM_DESTROY)
        invoke midiOutClose, hMidi
		invoke PostQuitMessage,NULL

	.ELSEIF (uMsg==WM_CREATE)
	    ; Open the midi stream device
        invoke  midiOutOpen, ADDR hMidi, MIDI_MAPPER, 0, 0, 0

        ; Create initial controls
        invoke  CreateWindowEx, WS_EX_TRANSPARENT, ADDR ClassStatic, ADDR strStcTuning, \
                WS_CHILD+WS_VISIBLE, 10, 13, 55, 24, hWnd, NULL, hInstance, NULL
        mov     hwndStcTuning, eax
        invoke  CreateWindowEx, WS_EX_TRANSPARENT, ADDR ClassButton, ADDR strChkLoopNote, \
                WS_CHILD+WS_VISIBLE+BS_CHECKBOX, 10, 40, 60, 24, hWnd, NULL, hInstance, NULL
        mov     hwndChkLoopNote, eax
        invoke  CreateWindowEx, NULL, ADDR ClassComboBox, NULL, \
                WS_CHILD+WS_VISIBLE+CBS_DROPDOWNLIST, 67, 10, 313, 300, hWnd, NULL, hInstance, NULL
        mov     hwndCbbTuningSelection, eax

        ;## Parse the tunings database and set initial conditions
        invoke  EnumerateTunings, ADDR strTuningStdGuitar, ADDR pTuningsIndex, TUNINGS
		invoke	InitSelector, hwndCbbTuningSelection, ADDR pTuningsIndex, TUNINGS
        invoke  SendMessage, hwndCbbTuningSelection, CB_SETCURSEL, 0, 0
        invoke	GetTuningByIndex, ADDR pTuningsIndex, 0
        invoke  CreateTunerButtons, hWnd, eax

    .ELSEIF (uMsg==WM_COMMAND)
        mov     eax, wParam
        shr     eax, 16
        .IF (ax==CBN_SELCHANGE)
            invoke	DestroyTunerButtons, hWnd, dwButtonCount
            invoke  SendMessage, hwndCbbTuningSelection, CB_GETCURSEL, 0, 0
        	invoke	GetTuningByIndex, ADDR pTuningsIndex, eax
            invoke  CreateTunerButtons, hWnd, eax
        .ELSEIF (ax==BN_CLICKED)
            invoke 	midiOutShortMsg, hMidi, dword ptr [AGUITAR] ; Change patch number
            invoke	CreateThread, NULL, NULL, ADDR PlayNote, lParam, NULL, ADDR hThread
            ;## Special handling for check boxes
            mov     eax, lParam
            .IF (eax==hwndChkLoopNote)
                invoke  SendMessage, hwndChkLoopNote, BM_GETCHECK, 0, 0
                .IF (eax==0)    ;## Unchecked
                    invoke  SendMessage, hwndChkLoopNote, BM_SETCHECK, 1, 0
                    or      dwTunerButtonStyle, BS_PUSHLIKE+BS_AUTOCHECKBOX
                    invoke	DestroyTunerButtons, hWnd, dwButtonCount
                    invoke  SendMessage, hwndCbbTuningSelection, CB_GETCURSEL, 0, 0
        	        invoke	GetTuningByIndex, ADDR pTuningsIndex, eax
                    invoke  CreateTunerButtons, hWnd, eax
                .ELSE           ;## Checked
                    invoke  SendMessage, hwndChkLoopNote, BM_SETCHECK, 0, 0
                    xor     eax, eax
                    or      eax, BS_PUSHLIKE+BS_AUTOCHECKBOX
                    not     eax
                    and     dwTunerButtonStyle, eax
                    invoke	DestroyTunerButtons, hWnd, dwButtonCount
                    invoke  SendMessage, hwndCbbTuningSelection, CB_GETCURSEL, 0, 0
        	        invoke	GetTuningByIndex, ADDR pTuningsIndex, eax
                    invoke  CreateTunerButtons, hWnd, eax
                .ENDIF
            .ENDIF
        .ENDIF

    .ELSEIF (uMsg==WM_CTLCOLORSTATIC)
        invoke  SetBkMode, wParam, TRANSPARENT
        invoke  SetTextColor, wParam, 0000000h
        mov     eax, hBkBrush
        ret

	.ELSE
		invoke DefWindowProc,hWnd,uMsg,wParam,lParam
		ret
	.ENDIF

	xor eax,eax
	ret
WndProc endp

CreateTunerButtons     proc    uses edx     hWnd:DWORD,pTuning:DWORD
  	LOCAL	pCurrentAddress:DWORD
  	LOCAL	dwStringCount:DWORD
  	LOCAL	dwCountDown:DWORD
  	LOCAL	dwPosX:DWORD
  	LOCAL   dwPosY:DWORD
  	LOCAL	pHandlePos:DWORD


	;## Preserve the starting address
	push	pTuning
	pop		pCurrentAddress

	;## Skip over the description string
	invoke	lstrlen, pCurrentAddress
	add		pCurrentAddress, eax
	inc		pCurrentAddress	;## Skip the null byte
	;## Store the string count
	mov		eax, pCurrentAddress
	push	dword ptr [eax]
	pop		dwStringCount
	push	dwStringCount
	pop		dwButtonCount
	add		pCurrentAddress, 4
	;## Create the buttons with the given string names
	xor		eax, eax
	xor		edx, edx
	mov		eax, 4
	mul		dwStringCount
	invoke	HeapAlloc, hHeap, HEAP_ZERO_MEMORY, eax
	mov		pWindowHandles, eax
	mov		pHandlePos, eax
	mov		dwPosX, 130
	mov     dwPosY, 60
   	push	dwStringCount
   	pop		dwCountDown
	.WHILE	(dwCountDown > 0)
        invoke  CreateWindowEx, NULL, ADDR ClassButton, pCurrentAddress, dwTunerButtonStyle, dwPosX, dwPosY, 30, 24, hWnd, NULL, hInstance, NULL
        mov		edx, pHandlePos
        mov		dword ptr [edx], eax
        add		pHandlePos, 4
        invoke	lstrlen, pCurrentAddress
        add		pCurrentAddress, eax
		inc		pCurrentAddress	;## Skip the null byte
		dec		dwCountDown
		add		dwPosX, 25
		add     dwPosY, 30
    .ENDW
    ;## Store the midi values in the button
    push	pWindowHandles
    pop		pHandlePos
   	push	dwStringCount
   	pop		dwCountDown
    .WHILE	(dwCountDown > 0)
    	mov		edx, pHandlePos
    	mov		eax, pCurrentAddress
        invoke  SetWindowLong, dword ptr [edx], GWL_USERDATA, dword ptr [eax]
        add		pHandlePos, 4
    	add		pCurrentAddress, 4
    	dec		dwCountDown
    .ENDW

    xor     eax, eax
    ret
CreateTunerButtons     endp

EnumerateTunings    proc    pTunings:DWORD,pIndex:DWORD,dwTunings:DWORD
  	LOCAL	pCurrentAddress:DWORD
  	LOCAL	dwStringCount:DWORD
  	LOCAL	dwCountDown:DWORD

	;## Preserve the starting address
	push	pTunings
	pop		pCurrentAddress

	;## Loop through the tunings
	.WHILE	(dwTunings > 0)
		;## Save this address in the index and increment index pointer
		push	pCurrentAddress
		mov		eax, pIndex
		pop		dword ptr [eax]
		add		pIndex, 4

		;## Skip over the description string
		invoke	lstrlen, pCurrentAddress
		add		pCurrentAddress, eax
		inc		pCurrentAddress	;## Skip the null byte
		;## Store the string count
		mov		eax, pCurrentAddress
		push	dword ptr [eax]
		pop		dwStringCount
		add		pCurrentAddress, 4
		;## Skip over the string names
	   	push	dwStringCount
	   	pop		dwCountDown
		.WHILE	(dwCountDown > 0)
			invoke	lstrlen, pCurrentAddress
			add		pCurrentAddress, eax
			inc		pCurrentAddress	;## Skip the null byte
			dec		dwCountDown
	    .ENDW
	    ;## Skip over the midi messages
	   	push	dwStringCount
	   	pop		dwCountDown
	    .WHILE	(dwCountDown > 0)
	    	add		pCurrentAddress, 4
	    	dec		dwCountDown
	    .ENDW
	    ;## Jump to next tuning address
	    mov     eax, pCurrentAddress
	    push    dword ptr [eax]
	    pop     pCurrentAddress
	    ;## Decrement tuner index counter
	    dec		dwTunings
	.ENDW


    ret
EnumerateTunings    endp

InitSelector	proc    uses edx    hWnd:DWORD,pIndex:DWORD,dwTunings:DWORD

	.WHILE	(dwTunings > 0)
		mov		eax, pIndex
		mov		edx, dword ptr [eax]
    	invoke  SendMessage, hWnd, CB_ADDSTRING, 0, edx
    	add		pIndex, 4
    	dec		dwTunings
	.ENDW

	ret
InitSelector 	endp

GetTuningByIndex	proc    uses edx    pIndex:DWORD,dwIndex:DWORD
	LOCAL	dwCount:DWORD

	push	dwIndex
	pop		dwCount

	.WHILE (dwCount > 0)
		add		pIndex, 4
		dec		dwCount
	.ENDW

	mov		edx, pIndex
	push	dword ptr [edx]
	pop		edx
	mov     eax, edx

	ret
GetTuningByIndex 	endp

DestroyTunerButtons	proc    hWnd:DWORD,dwStrings:DWORD
	LOCAL	pHandlePos:DWORD


	;## Store the window handle pointer
	push	pWindowHandles
	pop		pHandlePos

	;## Loop through and destroy the string button windows
	.WHILE (dwStrings > 0)
		mov		eax, pHandlePos
		invoke	DestroyWindow, dword ptr [eax]
		add		pHandlePos, 4
		dec		dwStrings
	.ENDW

	;## Destroy the handle pointer
	invoke	HeapFree, hHeap, 0, pWindowHandles

	ret
DestroyTunerButtons endp

PlayNote	proc	uses edx    hWnd:DWORD
    LOCAL   dwMidiMessage:DWORD
	LOCAL	dwMidiOffMessage:DWORD

	;## Get the midi data out of the button
    invoke 	GetWindowLong, hWnd, GWL_USERDATA
	mov     dwMidiMessage, eax

	;## Create a note-off midi message
	mov		eax, dwMidiMessage
	mov     edx, 0FFFFFF80h
	and     edx, eax
	mov     dwMidiOffMessage, edx

    ;## If loop is enabled and the button is checked then loop
    mov     eax, 1
    .WHILE (eax==1)
	    invoke 	midiOutShortMsg, hMidi, dwMidiMessage
        invoke 	Sleep, 2000
        invoke 	midiOutShortMsg, hMidi, dwMidiOffMessage
        invoke  SendMessage, hWnd, BM_GETCHECK, 0, 0
    .ENDW

	ret
PlayNote 	endp

szErrOnlyOneInstance    db  "You can only run one instance at a time.",0
CheckMultiInstance  proc    hInst:DWORD,pAppName:DWORD,pClassName:DWORD,pMutexName:DWORD

    invoke  CreateMutex, NULL, FALSE, pMutexName
    invoke  GetLastError
    .IF (eax==ERROR_ALREADY_EXISTS)
        ;invoke  MessageBox, NULL, ADDR szErrOnlyOneInstance, ADDR AppName, MB_OK+MB_ICONSTOP
        invoke  FindWindow, ADDR ClassName, NULL
        invoke  BringWindowToTop, eax
        mov     eax, FALSE
        ret
    .ENDIF

    mov     eax, TRUE

    ret
CheckMultiInstance  endp

szVerSubBlock           db  "\",0
szVerProductName        db  "\\StringFileInfo\\040904E4\\ProductName",0
szTmpAppTitle           db  "%s, v%lu.%lu.%lu.%lu",0
szTmpAppVersion         db  "%lu%lu%lu%lu",0
szTmpHttpHdrUserAgent   db  "User-Agent: %s/%lu.%lu.%lu.%lu",0dh,0ah,0
GetAppTitle proc    uses edx    hInst:DWORD,pAppName:DWORD,pBufferTitle:DWORD,pUserAgent:DWORD
    LOCAL   ModuleFileName[MAX_PATH]:BYTE
    LOCAL   VersionInfo[256]:BYTE
    LOCAL   pVersionBlock:DWORD
    LOCAL   VersionBlockSize:DWORD
    LOCAL   StrVerMS:DWORD
    LOCAL   StrVerLS:DWORD
    LOCAL   Ver1:DWORD
    LOCAL   Ver2:DWORD
    LOCAL   Ver3:DWORD
    LOCAL   Ver4:DWORD
    LOCAL   strAppVersion[16]:BYTE

    ;//Get the name of this file
    invoke  GetModuleFileName, hInst, ADDR ModuleFileName, MAX_PATH

    ;//Get the version info block
    invoke  GetFileVersionInfo, ADDR ModuleFileName, 0, 256, ADDR VersionInfo
    invoke  VerQueryValue, ADDR VersionInfo, ADDR szVerSubBlock, ADDR pVersionBlock, ADDR VersionBlockSize

    ;//Parse the info we need
    mov     edx, pVersionBlock
    assume  edx:ptr VS_FIXEDFILEINFO
    push    [edx].dwFileVersionLS
    push    [edx].dwFileVersionMS
    assume  edx:nothing
    pop     StrVerMS
    pop     StrVerLS

    xor     edx, edx
    mov     eax, StrVerMS
    mov     dx, ax
    mov     Ver2, edx
    xor     edx, edx
    shr     eax, 16
    mov     dx, ax
    mov     Ver1, edx

    xor     edx, edx
    mov     eax, StrVerLS
    mov     dx, ax
    mov     Ver4, edx
    xor     edx, edx
    shr     eax, 16
    mov     dx, ax
    mov     Ver3, edx

    ;//Put it into the app title buffer
    invoke  wsprintf, pBufferTitle, ADDR szTmpAppTitle, pAppName, Ver1, Ver2, Ver3, Ver4

    ;##: Create a user-agent string for http requests
    invoke  wsprintf, pUserAgent, ADDR szTmpHttpHdrUserAgent, pAppName, Ver1, Ver2, Ver3, Ver4

    ;//Create a DWORD out of the version number
    invoke  wsprintf, ADDR strAppVersion, ADDR szTmpAppVersion, Ver1, Ver2, Ver3, Ver4

    ;//Return the version number as a DWORD value in eax
    invoke  atodw, ADDR strAppVersion
    ret
GetAppTitle endp


end start
