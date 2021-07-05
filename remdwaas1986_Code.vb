' ****************************************************************
'						VISUAL PINBALL X
'						Escape from monkey island
'						first version by ianski
'						second version by Popotte
'						vpx version by Shoopity and Remdwaas1986
'						images and 3d objects provided by ianski, hauntfreaks, Shoopity and Remdwaas1986
'						Version 1.0.0
'						started 10-5-2021
' ****************************************************************

Option Explicit
Randomize

Const BallSize = 50	   ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1
Const SongVolume = 0.1 ' 1 is full volume. Value is from 0 to 1

' Load the core.vbs for supporting Subs and functions
LoadCoreFiles

Sub LoadCoreFiles
On Error Resume Next
	ExecuteGlobal GetTextFile("core.vbs")
	If Err Then MsgBox "Can't open core.vbs"
	ExecuteGlobal GetTextFile("controller.vbs")
	If Err Then MsgBox "Can't open controller.vbs"
	On Error Goto 0
End Sub

Sub startB2S(aB2S)
	If B2SOn Then
	Controller.B2SSetData 1,0
	Controller.B2SSetData 2,0
	Controller.B2SSetData 3,0
	Controller.B2SSetData 4,0
	Controller.B2SSetData 5,0
	Controller.B2SSetData 6,0
	Controller.B2SSetData 7,0
	Controller.B2SSetData 8,0
	Controller.B2SSetData 9,0
	Controller.B2SSetData 10,0
	Controller.B2SSetData 11,0
	Controller.B2SSetData 20,0
	Controller.B2SSetData 21,0
	Controller.B2SSetData 22,0
	Controller.B2SSetData 23,0
	Controller.B2SSetData 24,0
	Controller.B2SSetData 25,0
	Controller.B2SSetData 26,0
	Controller.B2SSetData 27,0
	Controller.B2SSetData 28,0
	Controller.B2SSetData 29,0
	Controller.B2SSetData 30,0
	Controller.B2SSetData 31,0
	Controller.B2SSetData 32,0
	Controller.B2SSetData 33,0
	Controller.B2SSetData 34,0
	Controller.B2SSetData 35,0
	Controller.B2SSetData 36,0
	Controller.B2SSetData 37,0
	Controller.B2SSetData 38,0
	Controller.B2SSetData 39,0
	Controller.B2SSetData 40,0
	Controller.B2SSetData 41,0
	Controller.B2SSetData 42,0
	Controller.B2SSetData 43,0
	Controller.B2SSetData 44,0
	Controller.B2SSetData 45,0
	Controller.B2SSetData 46,0
	Controller.B2SSetData 47,0
	Controller.B2SSetData 48,0
	Controller.B2SSetData 49,0
	Controller.B2SSetData 50,0
	Controller.B2SSetData 51,0
	Controller.B2SSetData 52,0
	Controller.B2SSetData 53,0
	Controller.B2SSetData 54,0
	Controller.B2SSetData 55,0
	Controller.B2SSetData 56,0
	Controller.B2SSetData 57,0
	Controller.B2SSetData 58,0
	Controller.B2SSetData 59,0
	Controller.B2SSetData 60,0
	Controller.B2SSetData 61,0
	Controller.B2SSetData 62,0
	Controller.B2SSetData 63,0
	Controller.B2SSetData 64,0
	Controller.B2SSetData 65,0
	Controller.B2SSetData 66,0
	Controller.B2SSetData 67,0
	Controller.B2SSetData 68,0
	Controller.B2SSetData 69,0
	Controller.B2SSetData 70,0
	Controller.B2SSetData 71,0
	Controller.B2SSetData 72,0
	Controller.B2SSetData 73,0
	Controller.B2SSetData 74,0
	Controller.B2SSetData 75,0
	Controller.B2SSetData 76,0
	Controller.B2SSetData 77,0
	Controller.B2SSetData 78,0
	Controller.B2SSetData 79,0
	Controller.B2SSetData 80,0
	Controller.B2SSetData 81,0
	Controller.B2SSetData 82,0
	Controller.B2SSetData 83,0
	Controller.B2SSetData 84,0
	Controller.B2SSetData 85,0
	Controller.B2SSetData 86,0
	Controller.B2SSetData 87,0
	Controller.B2SSetData 88,0
	Controller.B2SSetData 89,0
	Controller.B2SSetData 90,0
	Controller.B2SSetData 91,0
	Controller.B2SSetData 92,0
	Controller.B2SSetData 93,0
	Controller.B2SSetData 94,0
	Controller.B2SSetData 95,0
	Controller.B2SSetData 96,0
	Controller.B2SSetData 97,0
	Controller.B2SSetData 98,0
	Controller.B2SSetData 99,0
	Controller.B2SSetData aB2S,1
	End If
End Sub

' Define any Constants
Const cGameName = "monkeyisland"
Const TableName = "monkeyisland"
Const myVersion = "1.0.0"
Const MaxPlayers = 4	 ' from 1 to 4
Const BallSaverTime = 20 ' in seconds
Const MaxMultiplier = 3	 ' limit to 3x in this game, both bonus multiplier and playfield multiplier
Const BallsPerGame = 5	 ' usually 3 or 5
Const MaxMultiballs = 4	 ' max number of balls during multiballs

Const Special1 = 1000000  ' High score to obtain an extra ball/game
Const Special2 = 3000000
Const Special3 = 5000000

' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True then
	UseFlexDMD = False
Else
	UseFlexDMD = True
End If

' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)
Dim BonusMultiplier(4)
Dim bBonusHeld
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Special1Awarded(4)
Dim Special2Awarded(4)
Dim Special3Awarded(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim bAttractMode
Dim mBalls2Eject
Dim bAutoPlunger
Dim Insult
Dim Status

' Define Game Control Variables
Dim BallsOnPlayfield

' Define Game Flags
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
'Dim Multiball
Dim bMusicOn
Dim bJustStarted
Dim bJackpot
Dim plungerIM
Dim LastSwitchHit
dim countr
dim countr1
dim countr2
dim countr3
dim countr4
dim countr5
dim countr6
dim monkeybattle
dim treasuresfound
dim PFMultiplier
dim baropen
dim barunlocked
dim voodoounlocked
dim quest
dim mode1TimerCount
dim mode2TimerCount
dim bonustime
dim LowerFlippersActive
dim bananasearned
dim coinsearned
dim treasurfindy
dim imagechest
Dim bumperHits
Dim RovingMagnet

' core.vbs variables
Dim BSBarToBar, BSVoodooToVoodoo, BSLeChuck, BSTreasure
ReDim MyTroughLR(-1), MyTroughTreasure(-1), MyTroughLeChuck(-1)

' *********************************************************************
'				 Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
	LoadEM
	Dim i
	'Randomize
	'Impulse Plunger as autoplunger
	Const IMPowerSetting = 36 ' Plunger Power
	Const IMTime = 1.1		  ' Time in seconds for Full Plunge
	Set plungerIM = New cvpmImpulseP
	With plungerIM
		.InitImpulseP swplunger, IMPowerSetting, IMTime
		.Random 1.5
		.InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
		.CreateEvents "plungerIM"
	End With
	' Misc. VP table objects Initialisation, droptargets, animations...
	VPObjects_Init
	' load saved values, highscore, names, jackpot
	Loadhs
	'Init main variables
	For i = 1 To MaxPlayers
		Score(i) = 0
		BonusPoints(i) = 0
		BonusMultiplier(i) = 1
		BallsRemaining(i) = BallsPerGame
		ExtraBallsAwards(i) = 0
	Next
	' Initalise the DMD display
	DMD_Init
	' freeplay or coins
	bFreePlay = False 'we want coins
	'if bFreePlay = false Then DOF 125, DOFOn
	' Init main variables and any other flags
	bAttractMode = False
	bOnTheFirstBall = False
	bBallInPlungerLane = False
	bBallSaverActive = False
	bBallSaverReady = False
	bGameInPlay = False
	bMusicOn = True
	BallsOnPlayfield = 0
	bMultiBallMode = False
	'Multiball=false
	bAutoPlunger = False
	LastSwitchHit = ""
	Tilt = 0
	TiltSensitivity = 6
	Tilted = False
	bJustStarted = True
	' set any lights for the attract mode
	GiOff
	StartAttractMode
	'EndOfGame()
	Status = "Normal"
	'Initialize the magic dust
	Dim X
	For Each X in MagicDust
		X.Visible = 0
		X.X = Kicker002.X
		X.Y = Kicker002.Y
	Next
	FLMagic001.IntensityScale = 1
	FLMagic002.IntensityScale = 0.95
	FLMagic003.IntensityScale = 0.90
	FLMagic004.IntensityScale = 0.85
	FLMagic005.IntensityScale = 0.80
	FLMagic006.IntensityScale = 0.75
	FLMagic007.IntensityScale = 0.70
	FLMagic008.IntensityScale = 0.65
	FLMagic009.IntensityScale = 0.60
	FLMagic010.IntensityScale = 0.55
	FLMagic011.IntensityScale = 0.5
	FLMagic012.IntensityScale = 0.45
	FLMagic013.IntensityScale = 0.4
	FLMagic014.IntensityScale = 0.35
	FLMagic015.IntensityScale = 0.3
	FLMagic016.IntensityScale = 0.25
	FLMagic017.IntensityScale = 0.2
	FLMagic018.IntensityScale = 0.15
	FLMagic019.IntensityScale = 0.1
	FLMagic020.IntensityScale = 0.05
	'Voodoo magnet stuff
	Set RovingMagnet = New cvpmMagnet
	With RovingMagnet
		.InitMagnet TrMagnet, 3
		.X = Kicker002.X
		.Y = Kicker002.Y
		.Size = 75
		.GrabCenter = 0
	End With
	PirateShipTimer.Interval = 20
	Set BSBarToBar = New cvpmSaucer
	Set BSVoodooToVoodoo = New cvpmSaucer
	Set BSLeChuck = New cvpmSaucer
	Set BSTreasure = New cvpmSaucer
	With BSBarToBar
		.InitKicker Kicker006, 0, 160, 7, 0
		.InitSounds "fx_hole_enter", "fx_kicker", "fx_kicker"
	End With
	With BSVoodooToVoodoo
		.InitKicker Kicker005, 0, 190, 7, 0
		.InitSounds "fx_hole_enter", "fx_kicker", "fx_kicker"
	End With
	With BSLeChuck
		.InitKicker Kicker003, 0, 160, 7, 0
		.InitSounds "fx_hole_enter", "fx_kicker", "fx_kicker"
	End With
	With BSTreasure
		.InitKicker Kicker007, 0, 8, 38, 0
		.InitSounds "fx_hole_enter", "fx_kicker", "fx_kicker"
	End With
	schatkist001.Visible=1
	FlShipShadow.X = PirateShip.X
	FlShipShadow.Y = PirateShip.Y
End Sub

'****************************************
' Real Time updatess using the GameTimer
'****************************************
'used for all the real time updates
Dim Monkey1Frame, Monkey1HitRepeat, Repeat1, FrameNext1, FrameRate1b, FrameRate1c
'Starting Frame
Monkey1Frame = 1
'How many times HAVE we repeated the angry animation
Monkey1HitRepeat = 0
'How many times TO repeat the angry animation
Repeat1 = 1
'Speed of idle animation
FrameRate1c = 0.005
'Speed of Angry Animation
FrameRate1b = 0.05
'Speed of init Animation
FrameNext1 = FrameRate1c

Dim Monkey2Frame, Monkey2HitRepeat, Repeat2, FrameNext2, FrameRate2b, FrameRate2c
Monkey2Frame = 4
Monkey2HitRepeat = 0
Repeat2 = 1
FrameRate2c = 0.005
FrameRate2b = 0.05
FrameNext2 = FrameRate2c

Dim Monkey3Frame, Monkey3HitRepeat, Repeat3, FrameNext3, FrameRate3b, FrameRate3c
Monkey3Frame = 7
Monkey3HitRepeat = 0
Repeat3 = 1
FrameRate3c = 0.005
FrameRate3b = 0.05
FrameNext3 = FrameRate3c

Dim PullBack:PullBack = 0
Sub GameTimer_Timer
	Dim X, Y, tmpBall
	RollingUpdate
	' add any other real time update subs, like gates or diverters
	FlipperLSh.Rotz = LeftFlipper.CurrentAngle
	FlipperRSh.Rotz = RightFlipper.CurrentAngle
	'Rotate, raise, and dim the magic dust
	For Each X in MagicDust
		X.RotZ = X.RotZ + 1
		X.Height = X.Height + 0.75
		X.IntensityScale = X.IntensityScale - 0.01
		'If it's gone invisible, then send it back down to the bottom of the stack and set it totally visable
		If X.IntensityScale <= 0 Then
			X.Height = 25
			X.IntensityScale = 1
		End If
	Next
	'If we're in voodoo magic mode...
	If Status = "Magic" Then
		tmpBall = GetBalls
		For Each X in MagicDust
			'If the dust is at the bottom of the stack, then put it at wherever the ball is
			If X.Height = 25 Then
				For Each Y in tmpBall
					If Y.ID = 666 Then
						X.X = Y.X
						X.Y = Y.Y
						X.Height = Y.Z
					End If
				Next
			End If
		Next
	End If
	'Monkey Animations
	'Center Monkey
	'Show the current frame of animation
	Monkey1.ShowFrame(Monkey1Frame)
	'Go to the next frame of animation (including VPX interpolated frames)
	Monkey1Frame = Monkey1Frame + FrameNext1
	'Idle animation or angry animation
	Select Case Monkey1Hit
		'Idle
		Case 0
			'If we've reached the end or beginning of the animation...
			If Monkey1Frame > 9 OR Monkey1Frame < 1 Then
				'Change the direction of the frame step
				FrameNext1 = FrameNext1 * -1
			end if
		'Angry
		Case 1
			'If we've reached the end, then flip directions
			If Monkey1Frame > 19 Then FrameNext1 = FrameNext1 * -1
			'If we've reached the beginning...
			If Monkey1Frame < 11 Then
				'increment repeat count...
				Monkey1HitRepeat = Monkey1HitRepeat + 1
				'and flip directions
				FrameNext1 = FrameNext1 * -1
			end if
			'If we've repeated the desired amount...
			If Monkey1HitRepeat > Repeat1 Then
				'Reset the speed
				FrameNext1 = FrameRate1c
				'Reset to the first frame
				Monkey1Frame = 1
				'Back to idle
				Monkey1Hit = 0
				'Reset Repeat amount for the next time it gets hit
				Monkey1HitRepeat = 0
			End If
	End Select
	'Right Monkey
	Monkey2.ShowFrame(Monkey2Frame)
	Monkey2Frame = Monkey2Frame + FrameNext2
	Select Case Monkey2Hit
		Case 0
			If Monkey2Frame > 9 OR Monkey2Frame < 1 Then
				FrameNext2 = FrameNext2 * -1
			end if
		Case 1
			If Monkey2Frame > 19 Then FrameNext2 = FrameNext2 * -1
			If Monkey2Frame < 11 Then
				Monkey2HitRepeat = Monkey2HitRepeat + 1
				FrameNext2 = FrameNext2 * -1
			end if
			If Monkey2HitRepeat > Repeat2 Then
				FrameNext2 = FrameRate2c
				Monkey2Frame = 1
				Monkey2Hit = 0
				Monkey2HitRepeat = 0
			End If
	End Select
	'Left Monkey
	Monkey3.ShowFrame(Monkey3Frame)
	Monkey3Frame = Monkey3Frame + FrameNext3
	Select Case Monkey3Hit
		Case 0
			If Monkey3Frame > 9 OR Monkey3Frame < 1 Then
				FrameNext3 = FrameNext3 * -1
			end if
		Case 1
			If Monkey3Frame > 19 Then FrameNext3 = FrameNext3 * -1
			If Monkey3Frame < 11 Then
				Monkey3HitRepeat = Monkey3HitRepeat + 1
				FrameNext3 = FrameNext3 * -1
			end if
			If Monkey3HitRepeat > Repeat3 Then
				FrameNext3 = FrameRate3c
				Monkey3Frame = 1
				Monkey3Hit = 0
				Monkey3HitRepeat = 0
			End If
	End Select
	'Match the Kanon position to the plunger position
	kanon.TransX = Plunger.Position
'	If (ShipAngle > 160 AND ShipAngle < 200) OR (ShipAngle > 520 AND ShipAngle < 560) OR (ShipAngle > 880 AND ShipAngle < 920) OR (ShipAngle > 1240 AND ShipAngle < 1280) Then
'		TextBox001.Text = "NOW!!"
'	Else
'		TextBox001.Text = "wait"
'	end If
	TextBox001.Text = BallsOnPlayfield
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
	Dim Dummy
	'Test Kicker Stuff
	If Keycode = 20 Then															'EP- T	"Pick up" the ball
		If Not(BallMoverHold Is Nothing) Then
			If BallMoverHold.X = KiHold.X Then
				BallMoverHold.X = 425
				KiHold.Kick 0,0,0
			Else
				BallMoverHold.X = KiHold.X
				BallMoverHold.Y = KiHold.Y
				Trigger1_Unhit()
			End If
		End If
	End If
	If Keycode = 30 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -35, 30, 0			'EP- A	Eek shot
	If Keycode = 31 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -29, 50, 0			'EP- S	Left Loop
	If Keycode = 18 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -20, 30, 0			'EP- E	"I" target
	If Keycode = 33 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -15, 28, 0			'EP- F	Scumm Shot
	If Keycode = 34 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -11, 25, 0			'EP- G	"N" target
	If Keycode = 35 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -8, 40, 0			'EP- H	Scumm Ramp
	If Keycode = 36 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -3, 25, 0			'EP- J	"S" target
	If Keycode = 37 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -0.5, 40, 0			'EP- K	Le Chuck
	If Keycode = 38 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 5, 25, 0			'EP- L	Ack shot
	If Keycode = 39 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 12, 40, 0			'EP- ;	Monkey Head
	If Keycode = 40 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 19, 30, 0			'EP- '	Voodoo lady
	'If Keycode = 44 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 21, 30, 0			'EP- Z	"U" target
	If Keycode = 45 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 27, 60, 0			'EP- X	Banana ramp
	If Keycode = 46 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 31, 60, 0			'EP- C	"L" target
	If Keycode = 47 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 35, 60, 0			'EP- V	Grog/right loop
	If Keycode = 48 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 48, 30, 0			'EP- B	"T" target
	If Keycode = 49 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -70, 30, 0			'EP- N	left ball saver
	If Keycode = 50 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 70, 30, 0			'EP- N	right ball saver
	If Keycode = 25 Then SpinningWheel.enabled = 1:	WheelSpeed = 2					'EP- P	spin that wheel
	'***********************************************

	If Keycode = AddCreditKey Then
		Credits = Credits + 1
		if bFreePlay = False Then
			DOF 125, DOFOn
			If(Tilted = False) Then
				DMDFlush
				DMD "_", CL(1, "CREDITS: " & Credits), "", eNone, eNone, eNone, 500, True, "fx_coin"
			End If
		End If
	End If
	If keycode = PlungerKey Then
		Plunger.Pullback
		Playsound "plungerpull2"
		'PlaySoundAt "fx_plungerpull", plunger
		'PlaySoundAt "fx_reload", plunger
	End If
	If hsbModeActive Then
		EnterHighScoreKey(keycode)
		Exit Sub
	End If
	' Normal flipper action
	If bGameInPlay AND NOT Tilted Then
		If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
		If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
		If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt
'		If keycode = LeftFlipperKey Then SolLFlipper 1
'		If keycode = RightFlipperKey Then SolRFlipper 1
		If keycode = StartGameKey Then
			If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then
				If(bFreePlay = True) Then
					PlayersPlayingGame = PlayersPlayingGame + 1
					TotalGamesPlayed = TotalGamesPlayed + 1
					DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "so_fanfare1"
'					playsound "start"
				Else
					If(Credits> 0) then
						PlayersPlayingGame = PlayersPlayingGame + 1
						TotalGamesPlayed = TotalGamesPlayed + 1
						Credits = Credits - 1
						DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "so_fanfare1"
'						playsound "start"
						If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
					Else
						' Not Enough Credits to start a game.
						DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "so_nocredits"
						playsound "nomoney"
					End If
				End If
			End If
		End If
	Else ' If (GameInPlay)
		If keycode = StartGameKey Then
			If(bFreePlay = True) Then
				If(BallsOnPlayfield = 0) Then
					ResetForNewGame()
					UpdateMusicNow()
				End If
			Else
				If(Credits> 0) Then
					If(BallsOnPlayfield = 0) Then
						Credits = Credits - 1
						If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
						playsound "start"
						ResetForNewGame()
						UpdateMusicNow()

					End If
				Else
					' Not Enough Credits to start a game.
					DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "so_nocredits"
					playsound "nomoney"
				End If
			End If
		End If
	End If ' If (GameInPlay)
	If LowerFlippersActive Then
		If keycode = RightFlipperkey then
			PlaySound "fx_flipperup", 0, 1, 0, 0.25
			RightFlipper001.RotateToEnd
		end If
			If keycode = LeftFlipperkey then
			PlaySound "fx_flipperup", 0, 1, 0, 0.25
			LeftFlipper001.RotateToEnd
		end If
		Else
		If keycode = LeftFlipperKey Then SolLFlipper 1
		If keycode = RightFlipperKey Then SolRFlipper 1
	End If

'table keys
'If keycode = RightMagnaSave or keycode = LeftMagnasave Then ShowPost 
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then
		Plunger.Fire
	End If
	If hsbModeActive Then
		Exit Sub
	End If

	' Table specific

	If bGameInPLay AND NOT Tilted Then
		If keycode = LeftFlipperKey Then
			SolLFlipper 0
		End If
		If keycode = RightFlipperKey Then
			SolRFlipper 0
		End If
	End If

	If LowerFlippersActive Then
		If keycode = RightFlipperkey then
			PlaySound "fx_flipperup", 0, 1, 0, 0.25
			RightFlipper001.RotatetoStart
		end If
			If keycode = LeftFlipperkey then
			PlaySound "fx_flipperup", 0, 1, 0, 0.25
			LeftFlipper001.RotatetoStart
		end If
	End If

End Sub

'*************
' Pause Table
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub Table1_Exit
	Savehs
	If B2SOn = true Then Controller.Stop
End Sub

'********************
'	  Flippers
'********************

Sub SolLFlipper(Enabled)
	If Enabled Then
		PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
		LeftFlipper.RotateToEnd
'		Flipper1.RotateToEnd 'Adds To End Movement for Flipper1
		RotateLaneLightsLeft
		'RotateLaneLightsLeft2
	Else
		PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
		LeftFlipper.RotateToStart
'		Flipper1.RotateToStart 'Adds To End Movement for Flipper1
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
		RightFlipper.RotateToEnd
		RotateLaneLightsRight
		'RotateLaneLightsRight2
	Else
		PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
		RightFlipper.RotateToStart
	End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
	PlaySound "fx_rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
	PlaySound "fx_rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RotateLaneLightsLeft
	Dim TempState
	TempState = LeftOutlane.State
	LeftOutlane.State = LeftInlane.State
	LeftInlane.State = RightInlane.State
	RightInlane.State = RightOutlane.State
	RightOutlane.State = TempState
End Sub

Sub RotateLaneLightsRight
	Dim TempState
	TempState = RightOutlane.State
	RightOutlane.State = RightInlane.State
	RightInlane.State = LeftInlane.State
	LeftInlane.State = LeftOutlane.State
	LeftOutlane.State = TempState
End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt									 'Called when table is nudged
	Tilt = Tilt + TiltSensitivity				 'Add to tilt count
	TiltDecreaseTimer.Enabled = True
	If(Tilt> TiltSensitivity) AND(Tilt <15) Then 'show a warning
		DMD "_", CL(1, "CAREFUL!"), "", eNone, eBlinkFast, eNone, 500, True, "whathappend"
	End if
	If Tilt> 15 Then 'If more that 15 then TILT the table
		Tilted = True
		'display Tilt
		playSound "TILTY"
		DMDFlush
		DMD "", "", "TILT", eNone, eNone, eBlink, 200, False, ""
		DisableTable True
		TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
	End If
End Sub

Sub TiltDecreaseTimer_Timer
	' DecreaseTilt
	If Tilt> 0 Then
		Tilt = Tilt - 0.1
	Else
		TiltDecreaseTimer.Enabled = False
	End If
End Sub

Sub DisableTable(Enabled)
	If Enabled Then
		'turn off GI and turn off all the lights
		GiOff
		LightSeqTilt.Play SeqAllOff
		'Disable slings, bumpers etc
		LeftFlipper.RotateToStart
		RightFlipper.RotateToStart
		LeftSlingshot.Disabled = 1
		RightSlingshot.Disabled = 1
	Else
		'turn back on GI and the lights
		GiOn
		LightSeqTilt.StopPlay
		LeftSlingshot.Disabled = 0
		RightSlingshot.Disabled = 0
		'clean up the buffer display
		DMDFlush
	End If
End Sub

' GI light sequence effects

Sub GiEffect(n)
	Select Case n
		Case 0 'all blink
			LightSeqGi.UpdateInterval = 8
			LightSeqGi.Play SeqBlinking, , 5, 50
		Case 1 'random
			LightSeqGi.UpdateInterval = 10
			LightSeqGi.Play SeqRandom, 5, , 1000
		Case 2 'upon
			LightSeqGi.UpdateInterval = 4
			LightSeqGi.Play SeqUpOn, 5, 1
	End Select
End Sub

Sub LightEffect(n)
	Select Case n
		Case 0 'all blink
			LightSeqInserts.UpdateInterval = 8
			LightSeqInserts.Play SeqBlinking, , 5, 50
		Case 1 'random
			LightSeqInserts.UpdateInterval = 10
			LightSeqInserts.Play SeqRandom, 5, , 1000
		Case 2 'upon
			LightSeqInserts.UpdateInterval = 4
			LightSeqInserts.Play SeqUpOn, 10, 1
		Case 3 ' left-right-left
			LightSeqInserts.UpdateInterval = 5
			LightSeqInserts.Play SeqLeftOn, 10, 1
			LightSeqInserts.UpdateInterval = 5
			LightSeqInserts.Play SeqRightOn, 10, 1
	End Select
End Sub

Sub TiltRecoveryTimer_Timer()
	' if all the balls have been drained then..
	If(BallsOnPlayfield = 0) Then
		' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
		EndOfBall()
		TiltRecoveryTimer.Enabled = False
	End If
' else retry (checks again in another second or so)
End Sub

'********************
' Music as wav sounds
'********************

Dim Song, UpdateMusic
Song = ""

Sub PlaySong(name)
	If bMusicOn Then
		If Song <> name Then		'This if statement allows for the same song to play on top of itself; do we need it?
			StopSound Song
			Song = name
			PlaySound Song, -1, SongVolume
		End If
	End If
End Sub

Sub StopSong()
	If bMusicOn Then
		StopSound Song
		Song = ""
	End If
End Sub

Sub ChangeSong
	If(BallsOnPlayfield = 0)Then
		PlaySong "M_end"
		Exit Sub
	End If

	If bAttractMode Then
		PlaySong "M_end"
		Exit Sub
	End If
	If bMultiBallMode Then
		PlaySong "MULTI"
	Else
		UpdateMusicNow
	end if
End Sub

'if you add more balls to the game use changesong then if bMultiBallMode = true, your multiball song will be played.

Sub UpdateMusicNow()
	Select Case UpdateMusic
		Case 0:PlaySong "1"
		Case 1:PlaySong "2"
		Case 2:PlaySong "3"
		Case 3:PlaySong "4"
		Case 4:PlaySong "5"
		Case 5:PlaySong "M_end"
	End Select
end sub

'********************
' Play random quotes
'********************

Sub PlayLechuckQuote
	Dim tmp
	Randomize()
	tmp = INT(RND * 74) + 1
	PlaySound "Lechuck_" &tmp
End Sub

Sub PlayMurrayQuote
	Dim tmp
	Randomize()
	tmp = INT(RND * 88) + 1
	PlaySound "MUR_" &tmp
End Sub

Sub PlayQuoteInsult
	Dim tmp
	Randomize()
	tmp = INT(RND * 28) + 1
	PlaySound "INS_" &tmp
End Sub

Sub PlayQuoteVoodoo
	Dim tmp
	Randomize()
	tmp = INT(RND * 49) + 1
	PlaySound "voodool_" &tmp
End Sub

'**********************
'	  GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'**********************

Dim OldGiState
OldGiState = -1	  'start witht the Gi off

Sub ChangeGi(col) 'changes the gi color
	Dim bulb
	For each bulb in aGILights
		SetLightColor bulb, col, -1
	Next
End Sub

Sub GIUpdateTimer_Timer
	Dim tmp, obj
	tmp = Getballs
	If UBound(tmp) <> OldGiState Then
		OldGiState = Ubound(tmp)
		If UBound(tmp) = 1 Then 'we have 2 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
			GiOff				' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
		Else
			Gion
		End If
	End If
End Sub

Sub GiOn
	DOF 127, DOFOn
	Dim bulb
	For each bulb in aGiLights
		bulb.State = 1
	Next
	For each bulb in aBumperLights
		bulb.State = 1
	Next
' table1.ColorGradeImage = "ColorGradeLUT256x16_HalfSat"
End Sub

Sub GiOff
	DOF 127, DOFOff
	Dim bulb
	For each bulb in aGiLights
		bulb.State = 0
	Next
	For each bulb in aBumperLights
		bulb.State = 0
	Next
' table1.ColorGradeImage = "ColorGradeLUT256x16_HalfSat-dark"
End Sub

' GI, light & flashers sequence effects

Sub GiEffect(n)
	Dim ii
	Select Case n
		Case 0 'all off
			LightSeqGi.Play SeqAlloff
		Case 1 'all blink
			LightSeqGi.UpdateInterval = 10
			LightSeqGi.Play SeqBlinking, , 15, 10
		Case 2 'random
			LightSeqGi.UpdateInterval = 10
			LightSeqGi.Play SeqRandom, 50, , 1000
		Case 3 'all blink fast
			LightSeqGi.UpdateInterval = 10
			LightSeqGi.Play SeqBlinking, , 10, 10
		Case 4 'all blink once
			LightSeqGi.UpdateInterval = 10
			LightSeqGi.Play SeqBlinking, , 4, 1
	End Select
End Sub

Sub LightEffect(n)
	Select Case n
		Case 0 ' all off
			LightSeqInserts.Play SeqAlloff
		Case 1 'all blink
			LightSeqInserts.UpdateInterval = 10
			LightSeqInserts.Play SeqBlinking, , 15, 10
		Case 2 'random
			LightSeqInserts.UpdateInterval = 10
			LightSeqInserts.Play SeqRandom, 50, , 1000
		Case 3 'all blink fast
			LightSeqInserts.UpdateInterval = 10
			LightSeqInserts.Play SeqBlinking, , 10, 10
		Case 4 'up 1 time
			LightSeqInserts.UpdateInterval = 4
			LightSeqInserts.Play SeqUpOn, 8, 1
		Case 5 'up 2 times
			LightSeqInserts.UpdateInterval = 4
			LightSeqInserts.Play SeqUpOn, 8, 2
		Case 6 'down 1 time
			LightSeqInserts.UpdateInterval = 4
			LightSeqInserts.Play SeqDownOn, 8, 1
		Case 7 'down 2 times
			LightSeqInserts.UpdateInterval = 4
			LightSeqInserts.Play SeqDownOn, 8, 2
	End Select
End Sub

' *********************************************************************
'					   Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
	Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
	Dim tmp
	tmp = ball.x * 2 / table1.width-1
	If tmp > 0 Then
		Pan = Csng(tmp ^10)
	Else
		Pan = Csng(-((- tmp) ^10))
	End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
	Dim tmp
	tmp = ball.y * 2 / Table1.height-1
	If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
	Else
		AudioFade = Csng(-((- tmp) ^10))
	End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
	PlaySound soundname, 0, 1, Pan(tableobj), 0.06, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
	PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'********************************************
'	JP's VP10 Rolling Sounds + Ballshadow
' uses a collection of shadows, aBallShadow
'********************************************

Const tnob = 20 ' total number of balls
Const lob = 0	'number of locked balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim BOT, b, ballpitch, ballvol
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
		rolling(b) = False
		StopSound("fx_ballrolling" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

	' play the rolling sound for each ball and draw the shadow
	For b = lob to UBound(BOT)
		aBallShadow(b).X = BOT(b).X
		aBallShadow(b).Y = BOT(b).Y

		If BOT(b).Z < 0 Then aBallShadow(b).Height = BOT(b).Z -61

		If BallVel(BOT(b) )> 1 Then
			If BOT(b).z <30 Then
				ballpitch = Pitch(BOT(b) )
				ballvol = Vol(BOT(b) )
			Else
				ballpitch = Pitch(BOT(b) ) + 25000 'increase the pitch on a ramp
				ballvol = Vol(BOT(b) ) * 10
			End If
			rolling(b) = True
			PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b) ), 0, ballpitch, 1, 0, AudioFade(BOT(b) )
		Else
			If rolling(b) = True Then
				StopSound("fx_ballrolling" & b)
				rolling(b) = False
			End If
		End If
		' rothbauerw's Dropping Sounds
		If BOT(b).VelZ <-1 and BOT(b).z <55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
			PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
		End If
	Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound "fx_collide", 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

'Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
'Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
'Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
'Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
'Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
'Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
'Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

Sub trs001_Hit()
	PlaySound "fx_metalrolling"
end sub

Sub trs002_Hit()
	PlaySound "fx_metalrolling"
end sub

Sub trs003_Hit()
	PlaySound "fx_metalrolling"
end sub

Sub trs004_Hit()
	PlaySound "fx_metalrolling"
end sub

Sub trs005_Hit()
	PlaySound "fx_metalrolling"
end sub

Sub trs006_Hit()
	FlashForMs Flasher020, 1000, 50, 0
	PlaySound "fx_metalrolling"
end sub

Sub thelp001_Hit()
	RampBonus2 = RampBonus2 + 1
	FlashForMs Flasher003, 1000, 50, 0
	FlashForMs Flasher004, 1000, 50, 0
	StopSound "fx_metalrolling"
	PlaySound "fx_ballrampdrop"
End Sub

Sub thelp002_Hit()
	vpmtimer.addtimer 1000, "startB2S(3) '"
	RampBonus2 = RampBonus2 + 1
	baropen = baropen + 1 
	FlashForMs Flasher005, 1000, 50, 0
	FlashForMs Flasher006, 1000, 50, 0
	StopSound "fx_metalrolling"
	PlaySound "fx_ballrampdrop"
	if baropen > 4 then exit sub
	checkbarr()
End Sub

Sub thelp003_Hit()
	RampBonus2 = RampBonus2 + 1
	FlashForMs Flasher007, 1000, 50, 0
	FlashForMs Flasher008, 1000, 50, 0
	StopSound "fx_metalrolling"
	PlaySound "fx_ballrampdrop"
End Sub

sub checkbarr()
	if(baropen = 3) then
		itemrotytimer001.enabled = 1
		banaan001.Visible = 1
		tbanaan001.enabled = 1
		barunlocked = 1
		li048.state = 0
		If Status = "Normal" Then li007.state = 2
		PrScumm.Image = "Scumm Bar On"
	end if
end sub

' *********************************************************************
'						 User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
	startB2S(11)
	StopSong()
	PlaySound "wait"
	Dim i
	bGameInPLay = True
	'resets the score display, and turn off attract mode
	StopAttractMode
	GiOn
	TotalGamesPlayed = TotalGamesPlayed + 1
	CurrentPlayer = 1
	PlayersPlayingGame = 1
	bOnTheFirstBall = True
	'Multiball=false	
	For i = 1 To MaxPlayers
		Score(i) = 0
		BonusPoints(i) = 0
		'BonusHeldPoints(i) = 0
		BonusMultiplier(i) = 1
		BallsRemaining(i) = BallsPerGame
		ExtraBallsAwards(i) = 0
		Special1Awarded(i) = False
		Special2Awarded(i) = False
		Special3Awarded(i) = False
	Next
	' initialise any other flags
	Tilt = 0
	resetycoins
	'reset variables
	bumperHits = 100
	UpdateMusic = 0
'	BallInHoles = 0
	PFMultiplier = 1
	monkeybattle = 0
	treasuresfound = 0
	bananasearned = 0
	coinsearned = 0
	baropen = 0
	barunlocked = 0
	voodoounlocked = 0
	bonustime = 0
	imagechest = 0 
	'UpdateMusic = UpdateMusic + 6
	UpdateMusicNow
	' initialise Game variables
	Game_Init()
	' you may wish to start some music, play a sound, do whatever at this point
	DMD "", "", "rt", eNone, eNone, eNone, 5000, True, ""
	'reset games/timers
	stopquests()
	PrScumm.Image = "Scumm Bar Off"
	disablebanana
	countr6 = 0
	Updatechest()
	updateimagechest()
	vpmtimer.addtimer 5000, "FirstBall '"
	LeftOrbitStart = 0
	RightOrbitStart = 0
	GrogCounter = 0
	LeChuckHits = 0
	KiGrog.Enabled = 0
End Sub

' This is used to delay the start of a game to allow any attract sequence to
' complete.	 When it expires it creates a ball for the player to start playing with

Sub FirstBall()
	' reset the table for a new ball
	ResetForNewPlayerBall()
	' create a new ball in the shooters lane
	CreateNewBall()
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
	startB2S(2)
	LightSeq002.Play SeqUpOn, 25, 1000
	' make sure the correct display is upto date
	AddScore 0
	' set the current players bonus multiplier back down to 1X
	BonusMultiplier(CurrentPlayer) = 1
	'UpdateBonusXLights
	'reset any drop targets, lights, game Mode etc..
	'This is a new ball, so activate the ballsaver
	bBallSaverReady = True
	'Reset any table specific
	BumperBonus = 0
	HoleBonus = 0
	ALLRampBonus = 0
	RampBonus1 = 0
	RampBonus2 = 0
	RampBonus3 = 0
	MulitballBonus = 0
	Countr = 0
	bumperHits = 0
	Countr1 = 0
	Countr2 = 0
	Countr3 = 0
	Countr4 = 0
	Countr5 = 0
	ChickenChecker = 0
	DollChecker = 0
	EarChecker = 0
	MushroomChecker = 0
	treasurfindy = 0
	Status = "Normal"
	ResetNewBallVariables
	ResetNewBallLights()
	'Multiball=false	
End Sub

' Create a new ball on the Playfield
Sub CreateNewBall()
	LightSeqAttract.StopPlay
	' create a ball in the plunger lane kicker.
	BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass
	' There is a (or another) ball on the playfield
	BallsOnPlayfield = BallsOnPlayfield + 1
	' kick it out..
	PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
	BallRelease.Kick 90, 4
	'only this tableDrain / Plunger Functions
	'ChangeBallImage
	If BallsOnPlayfield > 1 Then
		bMultiBallMode = True
		bAutoPlunger = True
		'ChangeSong
	End If
End Sub

' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table
Sub AddMultiball(nballs)
	mBalls2Eject = mBalls2Eject + nballs
	CreateMultiballTimer.Enabled = True
	'and eject the first ball
	CreateMultiballTimer_Timer
End Sub

' Eject the ball after the delay, AddMultiballDelay
Sub CreateMultiballTimer_Timer()
	' wait if there is a ball in the plunger lane
	If bBallInPlungerLane Then
		Exit Sub
	Else
		If BallsOnPlayfield < MaxMultiballs Then
			CreateNewBall()
			mBalls2Eject = mBalls2Eject -1
			If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
				CreateMultiballTimer.Enabled = False
			End If
		Else 'the max number of multiballs is reached, so stop the timer
			mBalls2Eject = 0
			CreateMultiballTimer.Enabled = False
		End If
	End If
End Sub

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded

Sub EndOfBall()
	Dim BonusDelayTime
	' the first ball has been lost. From this point on no new players can join in
	bOnTheFirstBall = False
	startB2S(5)
	' only process any of this if the table is not tilted.	(the tilt recovery
	' mechanism will handle any extra balls or end of game)
	'LightSeqAttract.Play SeqBlinking, , 5, 150
	StopmodeEndofBall
	LightSeq001.StopPlay
	StopSong
	playsound "splash44"
	LightSeq003.Play SeqCircleOutOn, 25, 1
	'bonuscheckie
	Dim AwardPoints, TotalBonus, ii
	AwardPoints = 0
	TotalBonus = 0
	'If NOT Tilted Then
	If(Tilted = False) Then
		AwardPoints = TargetBonus * 2000
		TotalBonus = TotalBonus + AwardPoints
		DMD CL(0, FormatScore(AwardPoints)), CL(1, "TARGET BONUS " & TargetBonus), "", eBlink, eNone, eNone, 1000, False, "monkeyend"
		AwardPoints = RampBonus2 * 25000
		TotalBonus = TotalBonus + AwardPoints
		DMD CL(0, FormatScore(AwardPoints) ), CL(1, "RAMP BONUS: " & RampBonus2), "", eBlink, eNone, eNone, 1000, False, "monkeyend"
		AwardPoints = bananasearned * 10000
		TotalBonus = TotalBonus + AwardPoints
		DMD CL(0, FormatScore(AwardPoints) ), CL(1, "BANANA BONUS: " & bananasearned), "", eBlink, eNone, eNone, 1000, False, "monkeyend"
		AwardPoints = MulitballBonus * 150000
		TotalBonus = TotalBonus + AwardPoints
		AwardPoints = bumperHits * 10000
		TotalBonus = TotalBonus + AwardPoints
		DMD CL(0, FormatScore(AwardPoints) ), CL(1, "BARREL BONUS: " & BumperBonus), "", eBlink, eNone, eNone, 1000, False, "monkeyend"
		DMD CL(0, FormatScore(TotalBonus) ), CL(1, "TOTAL BONUS" & BonusMultiplier(CurrentPlayer) ), "", eBlinkFast, eNone, eNone, 1500, True, "monkeyend"
		TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
		AddScore TotalBonus
		' add a bit of a delay to allow for the bonus points to be shown & added up
		vpmtimer.addtimer 5700, "EndOfBall2 '"
	Else 'Si hay falta simplemente espera un momento y va directo a la segunta parte después de perder la bola
		BonusDelayTime = 100
		EndOfBall2
	End If
	'vpmtimer.addtimer BonusDelayTime, "EndOfBall2 '"
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.	Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()
	' if were tilted, reset the internal tilted flag (this will also
	' set TiltWarnings back to zero) which is useful if we are changing player LOL
	UpdateMusic = UpdateMusic + 1
	UpdateMusicNow	
	Tilted = False
	Tilt = 0
	DisableTable False 'enable again bumpers and slingshots
	' has the player won an extra-ball ? (might be multiple outstanding)
	If(ExtraBallsAwards(CurrentPlayer) <> 0) Then
		'debug.print "Extra Ball"
		' yep got to give it to them
		ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1
		' if no more EB's then turn off any shoot again light
		If(ExtraBallsAwards(CurrentPlayer) = 0) Then
			LightShootAgain.State = 0
		End If
		' You may wish to do a bit of a song AND dance at this point
		'DMD CL(0, "EXTRA BALL"), CL(1, "SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, "vo_extraball"
		DMD "", "", "dmdextraball", eNone, eNone, eNone, 1000, True, "fantastic"
		UpdateMusic = UpdateMusic - 1
		UpdateMusicNow
		' reset the playfield for the new ball
		ResetForNewPlayerBall()
		' set the dropped wall for bonus
		' Create a new ball in the shooters lane
		CreateNewBall()
	Else ' no extra balls
		BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1
		' was that the last ball ?
		If(BallsRemaining(CurrentPlayer) <= 0) Then
			'debug.print "No More Balls, High Score Entry"
			' Submit the CurrentPlayers score to the High Score system
			CheckHighScore()
		' you may wish to play some music at this point
		Else
			' not the last ball (for that player)
			' if multiple players are playing then move onto the next one
			EndOfBallComplete()
		End If
	End If
End Sub

' This function is called when the end of bonus display
' (or high score entry finished) AND it either end the game or
' move onto the next player (or the next ball of the same player)
'
Sub EndOfBallComplete()
	Dim NextPlayer

	'debug.print "EndOfBall - Complete"

	' are there multiple players playing this game ?
	If(PlayersPlayingGame> 1) Then
		' then move to the next player
		NextPlayer = CurrentPlayer + 1
		' are we going from the last player back to the first
		' (ie say from player 4 back to player 1)
		If(NextPlayer> PlayersPlayingGame) Then
			NextPlayer = 1
		End If
	Else
		NextPlayer = CurrentPlayer
	End If

	'debug.print "Next Player = " & NextPlayer

	' is it the end of the game ? (all balls been lost for all players)
	If((BallsRemaining(CurrentPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then
		' you may wish to do some sort of Point Match free game award here
		' generally only done when not in free play mode
		StopSong
		disablebanana
		startB2S(10)
		PlaySound "GAMEOVER"
		'DMD CL(0, "GAME OVER") "", eNone, 13000, True, ""
		DMD CL(0, "MONKEYS"), CL(1, "BATTLED " &monkeybattle), "", eNone, eNone, eNone, 750, True, ""
		DMD CL(0, "TREASURES"), CL(1, "FOUND " &treasuresfound), "", eNone, eNone, eNone, 750, True, ""
		DMD CL(0, "BANANAS"), CL(1, "CAUGHT " &bananasearned), "", eNone, eNone, eNone, 750, True, ""
		DMD CL(0, "COINS"), CL(1, "FOUND " &coinsearned), "", eNone, eNone, eNone, 750, True, ""
		DMD "", "", "dmdgameover", eNone, eNone, eNone, 2000, True, ""
		' set the machine into game over mode
		vpmtimer.addtimer 5000, "EndOfGame() '"
	' you may wish to put a Game Over message on the desktop/backglass
	Else
		' set the next player
		CurrentPlayer = NextPlayer
		' make sure the correct display is up to date
		DMDScoreNow
		' reset the playfield for the new player (or new ball)
		ResetForNewPlayerBall()
		' AND create a new ball
		CreateNewBall()
		' play a sound if more than 1 player
		If PlayersPlayingGame> 1 Then
			PlaySound "vo_player" &CurrentPlayer
			DMD "_", CL(1, "PLAYER " &CurrentPlayer), "", eNone, eNone, eNone, 800, True, ""
		End If
	End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
	LightSeqAttract.StopPlay
	'debug.print "End Of Game"
	bGameInPLay = False
	' just ended your game then play the end of game tune
	If NOT bJustStarted Then
		ChangeSong
	End If
	bJustStarted = False
	' ensure that the flippers are down
	SolLFlipper 0
	SolRFlipper 0
	' terminate all Mode - eject locked balls
	' most of the Mode/timers terminate at the end of the ball
	' set any lights for the attract mode
	GiOff
	StartAttractMode
	' you may wish to light any Game Over Light you may have
End Sub

Function Balls
	Dim tmp
	tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
	If tmp> BallsPerGame Then
		Balls = BallsPerGame
	Else
		Balls = tmp
	End If
End Function

' *********************************************************************
'					   Drain / Plunger Functions
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' if only one then decrement the remaining count AND test for End of game
' if more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
'
Sub Drain_Hit()
	'If this is a test ball, just destroy it and pretend like nothing happened
	If ActiveBall.ID = 21 Then Drain.DestroyBall:Exit Sub
	'If we're in voodoo Magic, do stuff to take us out
	If Status = "Magic" Then
		GetRidOfVoodooBall(ActiveBall)
		StopSong()
		UpdateMusicNow()
	End If
	' Destroy the ball
	Drain.DestroyBall
	BallsOnPlayfield = BallsOnPlayfield - 1 
	'If BallsOnPlayfield<2 Then
	'Multiball=false
	'end if
	' pretend to knock the ball into the ball storage mech
	PlaySoundAt "fx_drain", Drain
	'if Tilted then end Ball Mode
	If Tilted Then
		StopEndOfBallMode
	End If
	' if there is a game in progress AND it is not Tilted
	If(bGameInPLay = True) AND(Tilted = False) Then
		' is the ball saver active,
		If(bBallSaverActive = True) Then
			AddMultiball 1
			Playsound "fire"
			bAutoPlunger = True
			' yep, create a new ball in the shooters lane
			' we use the Addmultiball in case the multiballs are being ejected
			'DMD CL(0, "BALL SAVED"), CL(1, "SHOOT AGAIN"), "", eBlink, eBlink, eNone, 800, True, ""
			'vpmtimer.addtimer 1250, "CreateNewBall() '"
		   ' you may wish to put something on a display or play a sound at this point
		Else
			If(BallsOnPlayfield = 1)Then
				' AND in a multi-ball??
				If(bMultiBallMode = True)then
					' not in multiball mode any more
					bMultiBallMode = False
					' you may wish to change any music over at this point and
					' turn off any multiball specific lights
				ChangeSong()
				End If
			End If
			' was that the last ball on the playfield
			If(BallsOnPlayfield = 0) Then
				' End Mode and timers
				StopSong
				PlaySound ""
				'vpmtimer.addtimer 3000, "ChangeSong '"
				' Show the end of ball animation
				' and continue with the end of ball
				' DMD something?
				StopEndOfBallMode()
				vpmtimer.addtimer 200, "EndOfBall '" 'the delay is depending of the animation of the end of ball, since there is no animation then move to the end of ball
			End If
		End If
	End If
End Sub

' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.
Dim BallMoverHold:Set BallMoverHold = Nothing
Sub Trigger1_Hit()
	If bAutoPlunger Then
		PlungerIM.AutoFire
		DOF 121, DOFPulse
		PlaySound "fire"
		bAutoPlunger = False
	End If	
	'StopSong
'	DMDScoreNow()
	bBallInPlungerLane = True
'	DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eBlink, eNone, 1000, True, ""
	If (BallMoverHold Is Nothing) Then Set ballmoverhold = ActiveBall
	If bMultiBallMode = false Then PirateShipTimer.Enabled = 1
End Sub

' The ball is released from the plunger

Sub Trigger1_UnHit()
	bBallInPlungerLane = False
	'LightEffect 4
	'ChangeSong
	If PirateShipTimer.Enabled Then
		'If the ship is low then sink it
		If (ShipAngle > 160 AND ShipAngle < 200) OR (ShipAngle > 520 AND ShipAngle < 560) OR (ShipAngle > 880 AND ShipAngle < 920) OR (ShipAngle > 1240 AND ShipAngle < 1280) Then
			SinkPirateShip = 1
		Else
			'Bring the ship back to the start position
		End If
		smokeytimer.enabled = 1
	End If
	If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
		EnableBallSaver BallSaverTime
	Else
		' show the message to shoot the ball in case the player has fallen sleep
		Trigger1.TimerEnabled = 1
	End If
	Playsound "Fire"
End Sub

Sub Trigger1_Timer
'	DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eNone, eNone, 800, True, ""
	trigger1.TimerEnabled = 0
End Sub

Sub EnableBallSaver(seconds)
	'debug.print "Ballsaver started"
	' set our game flag
	bBallSaverActive = True
	bBallSaverReady = False
	' start the timer
	BallSaverTimer.Interval = 1000 * seconds
	BallSaverTimer.Enabled = True
	BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
	BallSaverSpeedUpTimer.Enabled = True
	' if you have a ball saver light you might want to turn it on at this point (or make it flash)
	LightShootAgain.BlinkInterval = 160
	LightShootAgain.State = 2
End Sub

' The ball saver timer has expired.	 Turn it off AND reset the game flag
'
Sub BallSaverTimer_Timer()
	'debug.print "Ballsaver ended"
	BallSaverTimer.Enabled = False
	' clear the flag
	bBallSaverActive = False
	' if you have a ball saver light then turn it off at this point
   LightShootAgain.State = 0
End Sub

Sub BallSaverSpeedUpTimer_Timer()
	'debug.print "Ballsaver Speed Up Light"
	BallSaverSpeedUpTimer.Enabled = False
	' Speed up the blinking
	LightShootAgain.BlinkInterval = 80
	LightShootAgain.State = 2
End Sub

' *********************************************************************
'					   Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
Sub AddScore(points)
	If Tilted Then Exit Sub

	' add the points to the current players score variable
	Score(CurrentPlayer) = Score(CurrentPlayer) + points

	' play a sound for each score
	PlaySound "tone"&points

	' you may wish to check to see if the player has gotten an extra ball by a high score
	If Score(CurrentPlayer) >= Special1 AND Special1Awarded(CurrentPlayer) = False Then
		AwardExtraBall
		Special1Awarded(CurrentPlayer) = True
	End If
	If Score(CurrentPlayer) >= Special2 AND Special2Awarded(CurrentPlayer) = False Then
		AwardExtraBall
		Special2Awarded(CurrentPlayer) = True
	End If
	If Score(CurrentPlayer) >= Special3 AND Special3Awarded(CurrentPlayer) = False Then
		AwardExtraBall
		Special3Awarded(CurrentPlayer) = True
	End If
End Sub

' Add bonus to the bonuspoints AND update the score board
Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
	If(Tilted = False) Then
		' add the bonus to the current players bonus variable
		BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
	End if
End Sub

Sub AwardExtraBall()
	DMD "", "", "dmdextraball", eNone, eNone, eNone, 1000, True, "fantastic"
	'DMD "_", CL(1, ("EXTRA BALL WON") ), "", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
	DOF 121, DOFPulse
	ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
	LightShootAgain.State = 1
	LightEffect 2
End Sub

'*****************************
'	 Load / Save / Highscore
'*****************************

Sub Loadhs
	Dim x
	x = LoadValue(TableName, "HighScore1")
	If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 100000 End If
	x = LoadValue(TableName, "HighScore1Name")
	If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
	x = LoadValue(TableName, "HighScore2")
	If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 100000 End If
	x = LoadValue(TableName, "HighScore2Name")
	If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
	x = LoadValue(TableName, "HighScore3")
	If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 100000 End If
	x = LoadValue(TableName, "HighScore3Name")
	If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
	x = LoadValue(TableName, "HighScore4")
	If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 100000 End If
	x = LoadValue(TableName, "HighScore4Name")
	If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
	x = LoadValue(TableName, "Credits")
	If(x <> "") then Credits = CInt(x) Else Credits = 0:If bFreePlay = False Then DOF 125, DOFOff:End If
	x = LoadValue(TableName, "TotalGamesPlayed")
	If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If
End Sub

Sub Savehs
	SaveValue TableName, "HighScore1", HighScore(0)
	SaveValue TableName, "HighScore1Name", HighScoreName(0)
	SaveValue TableName, "HighScore2", HighScore(1)
	SaveValue TableName, "HighScore2Name", HighScoreName(1)
	SaveValue TableName, "HighScore3", HighScore(2)
	SaveValue TableName, "HighScore3Name", HighScoreName(2)
	SaveValue TableName, "HighScore4", HighScore(3)
	SaveValue TableName, "HighScore4Name", HighScoreName(3)
	SaveValue TableName, "Credits", Credits
	SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

Sub Reseths
	HighScoreName(0) = "AAA"
	HighScoreName(1) = "BBB"
	HighScoreName(2) = "CCC"
	HighScoreName(3) = "DDD"
	HighScore(0) = 100000
	HighScore(1) = 110000
	HighScore(2) = 120000
	HighScore(3) = 130000
	Savehs
End Sub

' ***********************************************************
'  High Score Initals Entry Functions - based on Black's code
' ***********************************************************

Dim hsbModeActive
Dim hsEnteredName
Dim hsEnteredDigits(3)
Dim hsCurrentDigit
Dim hsValidLetters
Dim hsCurrentLetter
Dim hsLetterFlash

Sub CheckHighscore()
	Dim tmp
	tmp = Score(1)
	If Score(2)> tmp Then tmp = Score(2)
	If Score(3)> tmp Then tmp = Score(3)
	If Score(4)> tmp Then tmp = Score(4)

	'If tmp > HighScore(1)Then 'add 1 credit for beating the highscore
	'	 Credits = Credits + 1
	'	 DOF 125, DOFOn
	'End If

	If tmp> HighScore(3) Then
		PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
		DOF 121, DOFPulse
		HighScore(3) = tmp
		'enter player's name
		HighScoreEntryInit()
	Else
		EndOfBallComplete()
	End If
End Sub

Sub HighScoreEntryInit()
	hsbModeActive = True
	PlaySound "legend"
	ChangeSong
	hsLetterFlash = 0

	hsEnteredDigits(0) = " "
	hsEnteredDigits(1) = " "
	hsEnteredDigits(2) = " "
	hsCurrentDigit = 0

	'hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ'<>*+-/=\^0123456789`" ' ` is back arrow
	hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<" ' < is back arrow JP FLEX FIX
	hsCurrentLetter = 1
	DMDFlush()
	HighScoreDisplayNameNow()

	HighScoreFlashTimer.Interval = 250
	HighScoreFlashTimer.Enabled = True
End Sub

Sub EnterHighScoreKey(keycode)
	If keycode = LeftFlipperKey Then
		playsound "fx_Previous"
		hsCurrentLetter = hsCurrentLetter - 1
		if(hsCurrentLetter = 0) then
			hsCurrentLetter = len(hsValidLetters)
		end if
		HighScoreDisplayNameNow()
	End If

	If keycode = RightFlipperKey Then
		playsound "fx_Next"
		hsCurrentLetter = hsCurrentLetter + 1
		if(hsCurrentLetter> len(hsValidLetters) ) then
			hsCurrentLetter = 1
		end if
		HighScoreDisplayNameNow()
	End If

	If keycode = PlungerKey Then
		'if(mid(hsValidLetters, hsCurrentLetter, 1) <> "`") then
		if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<") then 'JP FLEX FIX
			playsound "fx_Enter"
			hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
			hsCurrentDigit = hsCurrentDigit + 1
			if(hsCurrentDigit = 3) then
				HighScoreCommitName()
			else
				HighScoreDisplayNameNow()
			end if
		else
			playsound "fx_Esc"
			hsEnteredDigits(hsCurrentDigit) = " "
			if(hsCurrentDigit> 0) then
				hsCurrentDigit = hsCurrentDigit - 1
			end if
			HighScoreDisplayNameNow()
		end if
	end if
End Sub

Sub HighScoreDisplayNameNow()
	HighScoreFlashTimer.Enabled = False
	hsLetterFlash = 0
	HighScoreDisplayName()
	HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreDisplayName()
	Dim i
	Dim TempTopStr
	Dim TempBotStr

	TempTopStr = "YOUR NAME:"
	dLine(0) = ExpandLine(TempTopStr, 0)
	DMDUpdate 0

	TempBotStr = "	  > "
	if(hsCurrentDigit> 0) then TempBotStr = TempBotStr & hsEnteredDigits(0)
	if(hsCurrentDigit> 1) then TempBotStr = TempBotStr & hsEnteredDigits(1)
	if(hsCurrentDigit> 2) then TempBotStr = TempBotStr & hsEnteredDigits(2)

	if(hsCurrentDigit <> 3) then
		if(hsLetterFlash <> 0) then
			TempBotStr = TempBotStr & "_"
		else
			TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
		end if
	end if

	if(hsCurrentDigit <1) then TempBotStr = TempBotStr & hsEnteredDigits(1)
	if(hsCurrentDigit <2) then TempBotStr = TempBotStr & hsEnteredDigits(2)

	TempBotStr = TempBotStr & " <	 "
	dLine(1) = ExpandLine(TempBotStr, 1)
	DMDUpdate 1
End Sub

Sub HighScoreFlashTimer_Timer()
	HighScoreFlashTimer.Enabled = False
	hsLetterFlash = hsLetterFlash + 1
	if(hsLetterFlash = 2) then hsLetterFlash = 0
	HighScoreDisplayName()
	HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreCommitName()
	HighScoreFlashTimer.Enabled = False
	hsbModeActive = False
	ChangeSong
	hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
	if(hsEnteredName = "   ") then
		hsEnteredName = "YOU"
	end if

	HighScoreName(3) = hsEnteredName
	SortHighscore
	EndOfBallComplete()
End Sub

Sub SortHighscore
	Dim tmp, tmp2, i, j
	For i = 0 to 3
		For j = 0 to 2
			If HighScore(j) <HighScore(j + 1) Then
				tmp = HighScore(j + 1)
				tmp2 = HighScoreName(j + 1)
				HighScore(j + 1) = HighScore(j)
				HighScoreName(j + 1) = HighScoreName(j)
				HighScore(j) = tmp
				HighScoreName(j) = tmp2
			End If
		Next
	Next
End Sub

' *************************************************************************
'	JP's Reduced Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and blinkfast
' 3 Lines, treats all 3 lines as text. 3rd line is just 1 character
' Example format:
' DMD "text1","text2","backpicture", eNone, eNone, eNone, 250, True, "sound"
' Short names:
' dq = display queue
' de = display effect
' *************************************************************************

Const eNone = 0		   ' Instantly displayed
Const eScrollLeft = 1  ' scroll on from the right
Const eScrollRight = 2 ' scroll on from the left
Const eBlink = 3	   ' Blink (blinks for 'TimeOn')
Const eBlinkFast = 4   ' Blink (blinks for 'TimeOn') at user specified intervals (fast speed)

Const dqSize = 64

Dim dqHead
Dim dqTail
Dim deSpeed
Dim deBlinkSlowRate
Dim deBlinkFastRate

Dim dCharsPerLine(2)
Dim dLine(2)
Dim deCount(2)
Dim deCountEnd(2)
Dim deBlinkCycle(2)

Dim dqText(2, 64)
Dim dqEffect(2, 64)
Dim dqTimeOn(64)
Dim dqbFlush(64)
Dim dqSound(64)

Dim FlexDMD
Dim DMDScene

Sub DMD_Init() 'default/startup values
	deSpeed = 20
    deBlinkSlowRate = 5
    deBlinkFastRate = 2
    dCharsPerLine(0) = 16 'characters top line
    dCharsPerLine(1) = 20 'characters lower line
    dCharsPerLine(2) = 1  'characters back line
    If UseFlexDMD Then
        Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
        If Not FlexDMD is Nothing Then
            FlexDMD.TableFile = Table1.Filename & ".vpx"
            FlexDMD.RenderMode = 2
            FlexDMD.Width = 128
            FlexDMD.Height = 32
            FlexDMD.Clear = True
            FlexDMD.GameName = cGameName
            FlexDMD.Run = True
            Set DMDScene = FlexDMD.NewGroup("Scene")
            DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkempty")
            DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
            For i = 0 to UBound(Digits)
                DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.dempty&dmd=2")
                Digits(i).Visible = False
            Next
            'digitgrid.Visible = False
            For i = 0 to (dCharsPerLine(1) - 1) ' Top
                DMDScene.GetImage("Dig" & i).SetBounds 4 + i * 6, dCharsPerLine(0) + 5, 8, 8
            Next
            For i = dCharsPerLine(1) to (dCharsPerLine(0) + dCharsPerLine(1) - 1) ' Bottom
                DMDScene.GetImage("Dig" & i).SetBounds (i - dCharsPerLine(1)) * 8, 3, 8, 16
            Next
            FlexDMD.LockRenderThread
            FlexDMD.Stage.AddActor DMDScene
            FlexDMD.UnlockRenderThread
        End If
    End If
    Dim i, j
    DMDFlush()
    For i = 0 to 2
        dLine(i) = Space(dCharsPerLine(i) )
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
        dqTimeOn(i) = 0
        dqbFlush(i) = True
        dqSound(i) = ""
    Next
    For i = 0 to 2
        For j = 0 to 64
            dqText(i, j) = ""
            dqEffect(i, j) = eNone
        Next
    Next
    DMD dLine(0), dLine(1), dLine(2), eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDFlush()
	Dim i
	DMDTimer.Enabled = False
	DMDEffectTimer.Enabled = False
	dqHead = 0
	dqTail = 0
	For i = 0 to 2
		deCount(i) = 0
		deCountEnd(i) = 0
		deBlinkCycle(i) = 0
	Next
End Sub

Sub DMDScore()
	Dim tmp, tmp1, tmp2
	if(dqHead = dqTail) Then
		tmp = RL(0, FormatScore(Score(Currentplayer)))
		tmp1 = CL(1, "PLAYER " & CurrentPlayer & "	BALL " & Balls)
		tmp2 = "bkborder"
	End If
	DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDScoreNow
	DMDFlush
	DMDScore
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
	if(dqTail < dqSize) Then
		if(Text0 = "_") Then
			dqEffect(0, dqTail) = eNone
			dqText(0, dqTail) = "_"
		Else
			dqEffect(0, dqTail) = Effect0
			dqText(0, dqTail) = ExpandLine(Text0, 0)
		End If
		if(Text1 = "_") Then
			dqEffect(1, dqTail) = eNone
			dqText(1, dqTail) = "_"
		Else
			dqEffect(1, dqTail) = Effect1
			dqText(1, dqTail) = ExpandLine(Text1, 1)
		End If
		if(Text2 = "_") Then
			dqEffect(2, dqTail) = eNone
			dqText(2, dqTail) = "_"
		Else
			dqEffect(2, dqTail) = Effect2
			dqText(2, dqTail) = Text2 'it is always 1 letter in this table
		End If
		dqTimeOn(dqTail) = TimeOn
		dqbFlush(dqTail) = bFlush
		dqSound(dqTail) = Sound
		dqTail = dqTail + 1
		if(dqTail = 1) Then
			DMDHead()
		End If
	End If
End Sub

Sub DMDHead()
	Dim i
	deCount(0) = 0
	deCount(1) = 0
	deCount(2) = 0
	DMDEffectTimer.Interval = deSpeed
	For i = 0 to 2
		Select Case dqEffect(i, dqHead)
			Case eNone:deCountEnd(i) = 1
			Case eScrollLeft:deCountEnd(i) = Len(dqText(i, dqHead) )
			Case eScrollRight:deCountEnd(i) = Len(dqText(i, dqHead) )
			Case eBlink:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
				deBlinkCycle(i) = 0
			Case eBlinkFast:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
				deBlinkCycle(i) = 0
		End Select
	Next
	if(dqSound(dqHead) <> "") Then
		PlaySound(dqSound(dqHead) )
	End If
	DMDEffectTimer.Enabled = True
End Sub

Sub DMDEffectTimer_Timer()
	DMDEffectTimer.Enabled = False
	DMDProcessEffectOn()
End Sub

Sub DMDTimer_Timer()
	Dim Head
	DMDTimer.Enabled = False
	Head = dqHead
	dqHead = dqHead + 1
	if(dqHead = dqTail) Then
		if(dqbFlush(Head) = True) Then
			DMDScoreNow()
		Else
			dqHead = 0
			DMDHead()
		End If
	Else
		DMDHead()
	End If
End Sub

Sub DMDProcessEffectOn()
    Dim i
    Dim BlinkEffect
    Dim Temp
    BlinkEffect = False
    For i = 0 to 2
        if(deCount(i) <> deCountEnd(i) ) Then
            deCount(i) = deCount(i) + 1
            select case(dqEffect(i, dqHead) )
                case eNone:
                    Temp = dqText(i, dqHead)
                case eScrollLeft:
                    Temp = Right(dLine(i), dCharsPerLine(i) - 1)
                    Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
                case eScrollRight:
					Temp = Mid(dqText(i, dqHead), (dCharsPerLine(i) + 1) - deCount(i), 1)
                    Temp = Temp & Left(dLine(i), dCharsPerLine(i) - 1)
                case eBlink:
                    BlinkEffect = True
                    if((deCount(i) MOD deBlinkSlowRate) = 0) Then
                        deBlinkCycle(i) = deBlinkCycle(i) xor 1
                    End If
                    if(deBlinkCycle(i) = 0) Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i) )
                    End If
                case eBlinkFast:
                    BlinkEffect = True
                    if((deCount(i) MOD deBlinkFastRate) = 0) Then
                        deBlinkCycle(i) = deBlinkCycle(i) xor 1
                    End If
                    if(deBlinkCycle(i) = 0) Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i) )
                    End If
            End Select

            if(dqText(i, dqHead) <> "_") Then
                dLine(i) = Temp
                DMDUpdate i
            End If
        End If
    Next
    if(deCount(0) = deCountEnd(0) ) and(deCount(1) = deCountEnd(1) ) and(deCount(2) = deCountEnd(2) ) Then
        if(dqTimeOn(dqHead) = 0) Then
            DMDFlush()
        Else
            if(BlinkEffect = True) Then
                DMDTimer.Interval = 10
            Else
                DMDTimer.Interval = dqTimeOn(dqHead)
            End If

            DMDTimer.Enabled = True
        End If
    Else
        DMDEffectTimer.Enabled = True
    End If
End Sub

Function ExpandLine(TempStr, id) 'id is the number of the dmd line
	If TempStr = "" Then
		TempStr = Space(dCharsPerLine(id) )
	Else
		if(Len(TempStr)> Space(dCharsPerLine(id) ) ) Then
			TempStr = Left(TempStr, Space(dCharsPerLine(id) ) )
		Else
			if(Len(TempStr) <dCharsPerLine(id) ) Then
				TempStr = TempStr & Space(dCharsPerLine(id) - Len(TempStr) )
			End If
		End If
	End If
	ExpandLine = TempStr
End Function

Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
	dim i
	dim NumString

	NumString = CStr(abs(Num) )

	For i = Len(NumString) -3 to 1 step -3
		if IsNumeric(mid(NumString, i, 1) ) then
			NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1) ) + 48) & right(NumString, Len(NumString) - i)
		end if
	Next
	FormatScore = NumString
End function

Function CL(id, NumString)
	Dim Temp, TempStr
	Temp = (dCharsPerLine(id) - Len(NumString) ) \ 2
	TempStr = Space(Temp) & NumString & Space(Temp)
	CL = TempStr
End Function

Function RL(id, NumString)
	Dim Temp, TempStr
	Temp = dCharsPerLine(id) - Len(NumString)
	TempStr = Space(Temp) & NumString
	RL = TempStr
End Function

'**************
' Update DMD
'**************

Sub DMDUpdate(id)
	Dim digit, value
	If UseFlexDMD Then FlexDMD.LockRenderThread
	Select Case id
		Case 0 'top text line
			For digit = dCharsPerLine(1) to (dCharsPerLine(0) + dCharsPerLine(1) - 1)
				DMDDisplayChar mid(dLine(0), digit - (dCharsPerLine(1) - 1), 1), digit
			Next
		Case 1 'bottom text line
			For digit = 0 to dCharsPerLine(1) - 1
				DMDDisplayChar mid(dLine(1), digit + 1, 1), digit
			Next
		Case 2 ' back image - back animations
			If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "bkempty"
			DigitsBack(0).ImageA = dLine(2)
			If UseFlexDMD Then DMDScene.GetImage("Back").Bitmap = FlexDMD.NewImage("", "VPX." & dLine(2) & "&dmd=2").Bitmap
	End Select
	If UseFlexDMD Then FlexDMD.UnlockRenderThread
End Sub

Sub DMDDisplayChar(achar, adigit)
	If achar = "" Then achar = " "
	achar = ASC(achar)
	Digits(adigit).ImageA = Chars(achar)
	If UseFlexDMD Then DMDScene.GetImage("Dig" & adigit).Bitmap = FlexDMD.NewImage("", "VPX." & Chars(achar) & "&dmd=2&add").Bitmap
End Sub

'****************************
' JP's new DMD using flashers
'****************************

Dim Digits, DigitsBack, Chars(255), Images(255)

DMDInit

Sub DMDInit()
	Dim i
	'If Table1.ShowDT = true then
	Digits = Array(digit0, digit1, digit2, digit3, digit4, digit5, digit6, digit7, digit8, digit9, digit10, digit11,				  _
		digit12, digit13, digit14, digit15, digit16, digit17, digit18, digit19, digit20, digit21, digit22, digit23, digit24, digit25, _
		digit26, digit27, digit28, digit29, digit30, digit31, digit32, digit33, digit34, digit35)
	DigitsBack = Array(digit36)
	For i = 0 to 255
		Chars(i)  = "dempty"
	Next '= "dempty":Images(i) = "dempty":Next
	Chars(32) = "dempty"
'	 Chars(34) = '"
'	 Chars(36) = '$
'	 Chars(39) = ''
'	 Chars(42) = '*
'	 Chars(43) = '+
'	 Chars(45) = '-
'	 Chars(47) = '/
	Chars(48) = "d0"	   '0
	Chars(49) = "d1"	   '1
	Chars(50) = "d2"	   '2
	Chars(51) = "d3"	   '3
	Chars(52) = "d4"	   '4
	Chars(53) = "d5"	   '5
	Chars(54) = "d6"	   '6
	Chars(55) = "d7"	   '7
	Chars(56) = "d8"	   '8
	Chars(57) = "d9"	   '9
	Chars(60) = "dless"	   '<
	Chars(61) = "dequal"   '=
	Chars(62) = "dgreater" '>
	'	Chars(64) = '@
	Chars(65) = "da" 'A
	Chars(66) = "db" 'B
	Chars(67) = "dc" 'C
	Chars(68) = "dd" 'D
	Chars(69) = "de" 'E
	Chars(70) = "df" 'F
	Chars(71) = "dg" 'G
	Chars(72) = "dh" 'H
	Chars(73) = "di" 'I
	Chars(74) = "dj" 'J
	Chars(75) = "dk" 'K
	Chars(76) = "dl" 'L
	Chars(77) = "dm" 'M
	Chars(78) = "dn" 'N
	Chars(79) = "do" 'O
	Chars(80) = "dp" 'P
	Chars(81) = "dq" 'Q
	Chars(82) = "dr" 'R
	Chars(83) = "ds" 'S
	Chars(84) = "dt" 'T
	Chars(85) = "du" 'U
	Chars(86) = "dv" 'V
	Chars(87) = "dw" 'W
	Chars(88) = "dx" 'X
	Chars(89) = "dy" 'Y
	Chars(90) = "dz" 'Z
	'Chars(91) = "dball" '[
	'Chars(92) = "dcoin" '|
	'Chars(93) = "dpika" ']
	'	 Chars(94) = '^
	'	 Chars(95) = '_
	Chars(96) = "d0a"  '0.
	Chars(97) = "d1a"  '1.
	Chars(98) = "d2a"  '2.
	Chars(99) = "d3a"  '3.
	Chars(100) = "d4a" '4.
	Chars(101) = "d5a" '5.
	Chars(102) = "d6a" '6.
	Chars(103) = "d7a" '7.
	Chars(104) = "d8a" '8.
	Chars(105) = "d9a" '9
End Sub

'********************************************************************************************
' Only for VPX 10.2 and higher.
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:	0=Off, 1=On, 2=Return to previous State
'********************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first version

	If TypeName(MyLight) = "Light" Then

		If FinalState = 2 Then
			FinalState = MyLight.State 'Keep the current light state
		End If
		MyLight.BlinkInterval = BlinkPeriod
		MyLight.Duration 2, TotalPeriod, FinalState
	ElseIf TypeName(MyLight) = "Flasher" Then

		Dim steps

		' Store all blink information
		steps = Int(TotalPeriod / BlinkPeriod + .5) 'Number of ON/OFF steps to perform
		If FinalState = 2 Then						'Keep the current flasher state
			FinalState = ABS(MyLight.Visible)
		End If
		MyLight.UserValue = steps * 10 + FinalState 'Store # of blinks, and final state

		' Start blink timer and create timer subroutine
		MyLight.TimerInterval = BlinkPeriod
		MyLight.TimerEnabled = 0
		MyLight.TimerEnabled = 1
		ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
	End If
End Sub

' #####################################
' ###### Flashers flupper #####
' #####################################

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object				***
Set TableRef = Table1			' *** change this, if your table has another name					***
FlasherLightIntensity = 1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 1		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherOffBrightness = 0.5		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20)
Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
'InitFlasher 1, "green" : InitFlasher 2, "red" : InitFlasher 3, "white"
'InitFlasher 4, "green" : InitFlasher 5, "red" : InitFlasher 6, "white"
'InitFlasher 7, "green" : 
InitFlasher 8, "yellow" : InitFlasher 9, "yellow" ': InitFlasher 10, "red" : InitFlasher 11, "white" 
' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 4,17 : RotateFlasher 5,0 : RotateFlasher 6,90
'RotateFlasher 7,0 : RotateFlasher 8,0 
'RotateFlasher 9,-45 : RotateFlasher 10,90 : RotateFlasher 11,90

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 35
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness
	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :	objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objlight(nr).intensity = 5000
		Case "green" :	objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4)
		Case "red" :	objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50)
		Case "white" :	objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 *	 FlasherFlareIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =	FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub 
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub 
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub 
Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub 
Sub FlasherFlash7_Timer() : FlashFlasher(7) : End Sub
Sub FlasherFlash8_Timer() : FlashFlasher(8) : End Sub
Sub FlasherFlash9_Timer() : FlashFlasher(9) : End Sub
Sub FlasherFlash10_Timer() : FlashFlasher(10) : End Sub
Sub FlasherFlash11_Timer() : FlashFlasher(11) : End Sub

' ###################################
' ###### copy script until here #####
' ###################################

' ***	   script for demoing flashers					***
' *** you should not need this in your table			***
' *** in your table start a flash with :				***
' *** ObjLevel(xx) = 1 : FlasherFlashxx_Timer			***
' *** for modulated flashers use 0-1 for ObjLevel(xx)	***

'dim countr : Randomize

'Sub Timer1_Timer
'	If TestFlashers = 0 Then
'		countr = countr + 1 : If Countr > 11 then Countr = 3 : end If
'		If rnd(1) < 0.04 Then
'			PlaySound "fx_relay_on",0,1
'			select case countr
				'case 1 : Objlevel(1) = 1 : FlasherFlash1_Timer
				'case 2 : Objlevel(2) = 1 : FlasherFlash2_Timer
				'case 3 : ObjLevel(3) = 1 : FlasherFlash3_Timer
				'case 4 : ObjLevel(4) = 1 : FlasherFlash4_Timer
				'case 5 : ObjLevel(5) = 1 : FlasherFlash5_Timer
				'case 6 : ObjLevel(6) = 1 : FlasherFlash6_Timer
				'case 7 : ObjLevel(7) = 1 : FlasherFlash7_Timer
				'case 8 : ObjLevel(8) = 1 : FlasherFlash8_Timer
'				case 9 : ObjLevel(9) = 1 : FlasherFlash9_Timer
				'case 10 : ObjLevel(10) = 1 : FlasherFlash10_Timer
				'case 11 : ObjLevel(11) = 1 : FlasherFlash11_Timer
'			end Select
'		End If
'	End If
'end Sub

' ********************************
'	Table info & Attract Mode
' ********************************

Sub ShowTableInfo
	Dim ii
	'info goes in a loop only stopped by the credits and the startkey
	If Score(1) Then
		DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 1" &FormatScore(Score(1) ) ), "", eNone, eNone, eNone, 3000, False, ""
	End If
	If Score(2) Then
		DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 2 " &FormatScore(Score(2) ) ), "", eNone, eNone, eNone, 3000, False, ""
	End If
	If Score(3) Then
		DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 3 " &FormatScore(Score(3) ) ), "", eNone, eNone, eNone, 3000, False, ""
	End If
	If Score(4) Then
		DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 4 " &FormatScore(Score(4) ) ), "", eNone, eNone, eNone, 3000, False, ""
	End If
		DMD CL(0, "GAME OVER"), CL(1, "TRY AGAIN"), "", eNone, eBlink, eNone, 2000, True, ""
	If bFreePlay Then
		DMD "", CL(1, "FREE PLAY"), "", eNone, eNone, eNone, 2000, False, ""
	Else
		If Credits> 0 Then
			DMD CL(0, "CREDITS " & Credits), CL(1, "PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
		Else
			DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
		End If
	End If
	DMD "", "", "dmdintro1", eNone, eNone, eNone, 2000, True, ""
	DMD "", "", "dmdintro2", eNone, eNone, eNone, 1000, True, ""
	DMD "", "", "dmdintrm1", eNone, eNone, eNone, 1000, True, ""
	DMD "", "", "dmdintrm2", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "dmdintrm3", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "dmdintrm4", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "dmdintrm5", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "dmdintrm6", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "dmdintrm7", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "dmdintrm8", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "dmdintrm9", eNone, eNone, eNone, 100, True, ""
	DMD "", "", "dmdintrm10", eNone, eNone, eNone, 500, True, ""
	DMD "", "", "dmdintrm11", eNone, eNone, eNone, 200, True, ""
	DMD "", "", "dmdintrm12", eNone, eNone, eNone, 200, True, ""
	DMD "", "", "dmdintrm13", eNone, eNone, eNone, 1000, True, ""
	DMD CL(0, "HIGHSCORES"), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
	DMD CL(0, "HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
	DMD CL(0, "HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
	DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
	DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
	DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
	DMD Space(dCharsPerLine(0) ), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Dim InAttract:InAttract = 0
Sub StartAttractMode
	startB2S(1)
	InAttract = 1
	spinningwheel.enabled = 1
	AttractTimer.Enabled = 1
	ChangeSong
	StartLightSeq
	DMDFlush
	ShowTableInfo
End Sub

Sub StopAttractMode
	InAttract = 0
	LightSeqAttract.StopPlay
	AttractTimer.Enabled = 0
	wheel1.Image = "Wheel1dark"
	wheel2.Image = "Wheel2dark"
	DMDScoreNow
End Sub

Sub AttractTimer_Timer
	Dim tmp
	Randomize()
	tmp = INT(RND * 8)
	Select Case tmp
		Case 0:PlaySound "attract01"
		Case 1:PlaySound "attract02"
		Case 2:PlaySound "attract03"
		Case 3:PlaySound "attract04"
		Case 4:PlaySound "attract05"
		Case 5:PlaySound "attract06"
		Case 6:PlaySound "attract07"
		Case 7:PlaySound "attract08"
	End Select
End Sub

Sub StartLightSeq()
	'lights sequences
	LightSeqAttract.UpdateInterval = 25
	LightSeqAttract.Play SeqBlinking, , 5, 150
	LightSeqAttract.Play SeqRandom, 40, , 4000
	LightSeqAttract.Play SeqAllOff
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqUpOn, 50, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqCircleOutOn, 15, 2
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqCircleOutOn, 15, 3
	LightSeqAttract.UpdateInterval = 5
	LightSeqAttract.Play SeqRightOn, 50, 1
	LightSeqAttract.UpdateInterval = 5
	LightSeqAttract.Play SeqLeftOn, 50, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqRightOn, 50, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqLeftOn, 50, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqRightOn, 40, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqLeftOn, 40, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqRightOn, 30, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqLeftOn, 30, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqRightOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqRightOn, 15, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqLeftOn, 15, 1
	LightSeqAttract.UpdateInterval = 10
	LightSeqAttract.Play SeqCircleOutOn, 15, 3
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqRightOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 5
	LightSeqAttract.Play SeqStripe1VertOn, 50, 2
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqCircleOutOn, 15, 2
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqStripe1VertOn, 50, 3
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqRightOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqCircleOutOn, 15, 2
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqStripe2VertOn, 50, 3
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqRightOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqLeftOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqUpOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqDownOn, 25, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqStripe1VertOn, 25, 3
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqStripe2VertOn, 25, 3
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqUpOn, 15, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqDownOn, 15, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqUpOn, 15, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqDownOn, 15, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqUpOn, 15, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqDownOn, 15, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqRightOn, 15, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqLeftOn, 15, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqRightOn, 15, 1
	LightSeqAttract.UpdateInterval = 8
	LightSeqAttract.Play SeqLeftOn, 15, 1
End Sub

Sub LightSeqAttract_PlayDone()
	StartLightSeq()
End Sub

Sub LightSeqTilt_PlayDone()
	LightSeqTilt.Play SeqAllOff
End Sub

'***********************************************************************
' *********************************************************************
'					  Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, etc
Sub VPObjects_Init
End Sub

' tables variables and Mode init
Dim HoleBonus, BumperBonus, ALLRampBonus, RampBonus1, RampBonus2, RampBonus3, MulitballBonus, TargetBonus	 

Sub Game_Init() 'called at the start of a new game
	Dim i, j
	ChangeSong
	TargetBonus = 0
	'bumperHits = 100
	BumperBonus = 0
	ALLRampBonus = 0
	RampBonus1 = 0
	RampBonus2 = 0
	RampBonus3 =0
	MulitballBonus = 0
	'BallInHole = 0
	TurnOffPlayfieldLights()
	Status = "Normal"
	in_ = "**"
	su_ = "**"
	lt_ = "**"
End Sub

Sub StopEndOfBallMode()		'this sub is called after the last ball is drained
	
End Sub

Sub ResetNewBallVariables() 'reset variables for a new ball or player
	Dim i
	TargetBonus = 0
	bBallSaverReady = True
End Sub

Sub ResetNewBallLights()	'turn on or off the needed lights before a new ball is released
	if voodoounlocked = 0 then
		li047.state = 1 
		li011.state = 0
	end if
	if barunlocked = 0 then
		li048.state = 1
		li007.state = 0
	end if	
	'TurnOffPlayfieldLights
	'li025.State = 1
	'li021.State = 1
	'li022.State = 1
	'li023.State = 1
	'li024.State = 1
	'li033.state = 1
	gi1.state = 1
	gi2.state = 1
	gi3.state = 1
	gi4.state = 1
End Sub

Sub TurnOffPlayfieldLights()
	Dim a
	For each a in aLights
		a.State = 0
	Next
End Sub

' *********************************************************************
'						 Table Object Hit Events
'
' Any target hit Sub should do something like this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/Mode this trigger is a member of
' - set the "LastSwitchHit" variable in case it is needed later
' *********************************************************************

'************
' Slingshots
'************

Dim RStep, Lstep

Sub RightSlingShot_Slingshot

	PlaySound SoundFX("rightslingy",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
	RSling.Visible = 0:RSling1.Visible = 1
	sling1.rotx = 20
	RStep = 0
	RightSlingShot.TimerEnabled = 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (210*PFMultiplier)
	gi1.State = 0
	Gi2.State = 0	
End Sub

Sub RightSlingShot_Timer
	Select Case RStep
		Case 1:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10:gi1.State = 0:Gi2.State = 0
		Case 1:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 5:gi1.State = 0:Gi2.State = 0
		Case 2:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:gi1.State = 1:Gi2.State = 1:RightSlingShot.TimerEnabled = False
	End Select
	RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	PlaySound SoundFX("leftslingy",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
	LSling.Visible = 0:LSling1.Visible = 1
	sling2.rotx = 20
	 LStep = 0
	LeftSlingShot.TimerEnabled = 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	gi3.State = 0
	Gi4.State = 0
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 1:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10:gi3.State = 0:Gi4.State = 0
		Case 2:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 5:gi3.State = 0:Gi4.State = 0
		Case 3:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:gi3.State = 1:Gi4.State = 1:LeftSlingShot.TimerEnabled = False
	End Select
	LStep = LStep + 1
End Sub

'*****************
'Spinners
'*****************

Sub Spinner001_Spin()
	PlaySound "tin can"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (500*PFMultiplier)
End Sub

'*****************
'triggers
'*****************
Dim LeftOrbitStart:LeftOrbitStart = 0
Dim RightOrbitStart:RightOrbitStart = 0
Dim GrogCounter:GrogCounter = 0
Dim GrogEnabled:GrogEnabled = 1

Sub TrRightOrbit_Hit()
	If KiGrog.Enabled = 1 Then
		Exit Sub
	End If
	If RightOrbitStart = 1 Then
		'Play Some Sound that you didn't make it
		RightOrbitStart = 0
		DMDFlush()
		DMD "", CL(1, "<0>"), "_", eNone, eNone, eNone, 1, True, ""
		DMD "", CL(1, " "), "_", eNone, eScrollRight, eNone, 1, True, ""
		Exit Sub
	End If
	If LeftOrbitStart = 1 Then
		LeftOrbitStart = 0
		Exit Sub
	End If
	DMD "", CL(1, "<0>"), "_", eNone, eScrollLeft, eNone, 1, True, ""
	DMD "", CL(1, " "), "_", eNone, eScrollLeft, eNone, 1, True, ""
	RightOrbitStart = 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (100*PFMultiplier)
	LSOrbit.Play SeqRadarLeftOn, 10
End Sub

Sub TrRightOrbitMake_Hit()
	If RightOrbitStart = 1 Then
		Score(CurrentPlayer) = Score(CurrentPlayer) + (900*PFMultiplier)
		'Play Some Sound for making a right orbit
		'Maybe some lighting effect
		DMD CL(1, "<0>"), "", "_", eScrollLeft, eNone, eNone, 1, True, ""
		DMD CL(1, " "), "", "_", eScrollLeft, eNone, eNone, 1, True, ""
		GrogCounter = GrogCounter + 1
	End If
	If GrogCounter >= 3 Then
		GrogEnabled = 1
'		KiGrog.Enabled = 1
	End If
End Sub

Sub TrLeftOrbit_Hit()
	If LeftOrbitStart = 1 Then
		'Play Some Sound that you didn't make it
		LeftOrbitStart = 0
		DMDFlush()
		DMD "", CL(1, "<0>"), "_", eNone, eNone, eNone, 1, True, ""
		DMD "", CL(1, " "), "_", eNone, eScrollLeft, eNone, 1, True, ""
		Exit Sub
	End If 
	If RightOrbitStart = 1 Then
		RightOrbitStart = 0
		Exit Sub
	End If
	DMD "_", CL(1, "<0>"), "_", eNone, eScrollRight, eNone, 1, True, ""
	DMD "_", CL(1, ""), "_", eNone, eScrollRight, eNone, 1, True, ""
	LeftOrbitStart = 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (100*PFMultiplier)
	LSOrbit.Play SeqRadarRightOn, 10
End Sub

Sub TrLeftOrbitMake_Hit()
	If LeftOrbitStart = 1 Then
		Score(CurrentPlayer) = Score(CurrentPlayer) + (900*PFMultiplier)
		'Play Some Sound for making a right orbit
		'Maybe some lighting effect
		DMD CL(0, "<0>"), "_", "_", eScrollRight, eNone, eNone, 1, True, ""
		DMD CL(0, ""), "_", "_", eScrollRight, eNone, eNone, 1, True, ""
	End If
End Sub

'**********************Bananas b2s bonus *********************

sub itemrotytimer001_timer
   banaan001.RotY = banaan001.RotY + 1
   if banaan001.RotY > 360 then
	   banaan001.RotY = 1
   end if
   banaan002.RotY = banaan002.RotY + 1
   if banaan002.RotY > 360 then
	   banaan002.RotY = 1
   end if
end sub

sub banaany1()
	tbanaan001.enabled = 1
	tbanaan002.enabled = 0
	banaan001.Visible = 1
	banaan002.Visible = 0
end sub

sub banaany2()
	tbanaan001.enabled = 0
	tbanaan002.enabled = 1
	banaan001.Visible = 0
	banaan002.Visible = 1
end sub

sub tbanaan001_hit()
	playsound "haveabanana"
	banaany2()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (5000*PFMultiplier)
	bananasearned = bananasearned + 1
	Updatebananascountr()
	vpmtimer.addtimer 250, "startB2S(3) '"
end sub

sub tbanaan002_hit()
	playsound "bananashurt"
	banaany1()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (5000*PFMultiplier)
	bananasearned = bananasearned + 1
	Updatebananascountr()
	vpmtimer.addtimer 250, "startB2S(3) '"
end sub

sub disablebanana()
	itemrotytimer001.enabled = 0
	banaan002.Visible = 0
	banaan001.Visible = 0
	tbanaan002.enabled = 0
	tbanaan001.enabled = 0
end sub

'**********************murray cave*********************

Sub tmurray_Hit
startB2S(6)
PlayMurrayQuote
End Sub

'**********************inner/outerlane*********************

Sub TLeftInlane_Hit
	FlashForMs Flasher011, 1000, 50, 0
	LeftInlane.State = 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	PlaySound "doorclose"
	DMD "", "", "10k", eNone, eNone, eNone, 500, True, ""
	Checkbgrog
End Sub

Sub TLeftOutlane_Hit
	ObjLevel(8) = 1 : FlasherFlash8_Timer
	FlashForMs Flasher011, 1000, 50, 0
	LeftOutlane.State = 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (50000*PFMultiplier)
	PlaySound "wallop"
	DMD "", "", "50k", eNone, eNone, eNone, 500, True, ""
	Checkbgrog
End Sub

Sub TRightInlane_Hit
	FlashForMs Flasher012, 1000, 50, 0
RightInlane.State = 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	PlaySound "doorclose"
	DMD "", "", "10k", eNone, eNone, eNone, 500, True, ""
	Checkbgrog
End Sub

Sub TRightOutlane_Hit
	ObjLevel(9) = 1 : FlasherFlash9_Timer
	FlashForMs Flasher012, 1000, 50, 0
	RightOutlane.State = 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (50000*PFMultiplier)
	PlaySound "wallop"
	DMD "", "", "50k", eNone, eNone, eNone, 500, True, ""
	Checkbgrog
End Sub

Sub Checkbgrog
	If(LeftInlane.State = 1) And(LeftOutlane.State = 1) And(RightInlane.State = 1) And(RightOutlane.State = 1) Then
	DMD "", "", "dmdgrog", eNone, eNone, eNone, 1000, True, "grog"
	AddScore 100000
	LeftInlane.State=0
	LeftOutlane.State=0
	RightInlane.State=0
	RightOutlane.State=0	  
	End If
End Sub

'**********************other way to add bonus*********************

Sub Bonuschecker_Hit
	ObjLevel(9) = 1 : FlasherFlash9_Timer
	ObjLevel(8) = 1 : FlasherFlash8_Timer
	FlashForMs Flasher001, 1000, 50, 0
	FlashForMs Flasher002, 1000, 50, 0
	FlashForMs Flasher003, 1000, 50, 0
	FlashForMs Flasher004, 1000, 50, 0
	FlashForMs Flasher005, 1000, 50, 0
	FlashForMs Flasher006, 1000, 50, 0
	FlashForMs Flasher007, 1000, 50, 0
	FlashForMs Flasher008, 1000, 50, 0
	FlashForMs Flasher009, 1000, 50, 0
	FlashForMs Flasher011, 1000, 50, 0
	FlashForMs Flasher012, 1000, 50, 0
	FlashForMs Flasher020, 1000, 50, 0
	FlashForMs Flasher019, 1000, 50, 0
End Sub

'************************** 
'Bumpers 
'************************** 

sub scorebumpers
PlaySound "shakegrog"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (2500*PFMultiplier)
end sub	

Sub Bumper001_hit()
bumperHits = bumperHits + 1
	DMDFlush
	DMD CL(0, FormatScore(Score(Currentplayer) ) ), CL(1, bumperHits& " BARRELS HIT"), "_", eNone, eNone, eNone, 300, True, ""
	ObjLevel(9) = 1 : FlasherFlash9_Timer
	ObjLevel(8) = 1 : FlasherFlash8_Timer
	PlaySound "BlockHi"
	Bumper001tim.Enabled = 1
'	Me.TimerEnabled = 1
	TipFull1 = 8
	HasJumped1 = 0
	scorebumpers
End sub
' Bumper Bonus
' 100000 i bonus after each 100 hits

Dim Tip1, TipFull1, Jump1, HasJumped1, JumpSpeed
Jumpspeed = 1
Jump1 = 1
Tip1 = 1
TipFull1 = 8
Sub Bumper001tim_Timer()
	If HasJumped1 = 0 Then
		barrel001.Z = dSin(Jump1)*30 + 100
		Jump1 = Jump1 + 1
		If Jump1 >= 180 Then
			Jump1 = 1
			HasJumped1 = 1
		End If
	End If
	If HasJumped1 = 1 Then
		barrel001.RotY = dSin(Tip1)*TipFull1
		barrel001.ObjRotZ = barrel001.ObjRotZ + 0.1
		If barrel001.ObjRotZ >= 360 Then barrel001.ObjRotZ = 0
		Tip1 = Tip1 + 1
		If Tip1 > 360 Then
			Tip1 = 1
			TipFull1 = TipFull1 - 1
		End If
		If TipFull1 <= 0 Then Bumper001tim.Enabled = 0
	End If
End Sub

Sub Bumper002_hit()
bumperHits = bumperHits + 1
	DMDFlush
	DMD CL(0, FormatScore(Score(Currentplayer) ) ), CL(1, bumperHits& " BARRELS HIT"), "_", eNone, eNone, eNone, 300, True, ""
	ObjLevel(9) = 1 : FlasherFlash9_Timer
	ObjLevel(8) = 1 : FlasherFlash8_Timer
	PlaySound "BlockMid"
	Bumper002tim.Enabled = 1
'	Me.TimerEnabled = 1
	TipFull2 = 8
	HasJumped2 = 0
	scorebumpers
End sub
' Bumper Bonus
' 100000 i bonus after each 100 hits

Dim Tip2, TipFull2, Jump2, HasJumped2
Jump2 = 1
Tip2 = 1
TipFull2 = 8
Sub Bumper002tim_Timer()
	If HasJumped2 = 0 Then
		barrel002.Z = dSin(Jump2)*30 + 100
		Jump2 = Jump2 + 1
		If Jump2 >= 180 Then
			Jump2 = 1
			HasJumped2 = 1
		End If
	End If
	If HasJumped2 = 1 Then
		barrel002.RotY = dSin(Tip2)*TipFull2
		barrel002.ObjRotZ = barrel002.ObjRotZ + 0.1
		If barrel002.ObjRotZ >= 360 Then barrel002.ObjRotZ = 0
		Tip2 = Tip2 + 1
		If Tip2 > 360 Then
			Tip2 = 1
			TipFull2 = TipFull2 - 1
		End If
		If TipFull2 <= 0 Then Bumper002tim.Enabled = 0
	End If
End Sub

Sub Bumper003_hit()
bumperHits = bumperHits + 1
	DMDFlush
	DMD CL(0, FormatScore(Score(Currentplayer) ) ), CL(1, bumperHits& " BARRELS HIT"), "_", eNone, eNone, eNone, 300, True, ""
	ObjLevel(9) = 1 : FlasherFlash9_Timer
	ObjLevel(8) = 1 : FlasherFlash8_Timer
	PlaySound "BlockLo"
	Bumper003tim.Enabled = 1
'	Me.TimerEnabled = 1
	TipFull3 = 8
	HasJumped3 = 0
	scorebumpers
End sub
' Bumper Bonus
' 100000 i bonus after each 100 hits

Dim Tip3, TipFull3, Jump3, HasJumped3
Jump3 = 1
Tip3 = 1
TipFull3 = 8
Sub Bumper003tim_Timer()
	If HasJumped3 = 0 Then
		barrel003.Z = dSin(Jump3)*30 + 100
		Jump3 = Jump3 + 1
		If Jump3 >= 180 Then
			Jump3 = 1
			HasJumped3 = 1
		End If
	End If
	If HasJumped3 = 1 Then
		barrel003.RotY = dSin(Tip3)*TipFull3
		barrel003.ObjRotZ = barrel003.ObjRotZ + 0.1
		If barrel003.ObjRotZ >= 360 Then barrel003.ObjRotZ = 0
		Tip3 = Tip3 + 1
		If Tip3 > 360 Then
			Tip3 = 1
			TipFull3 = TipFull3 - 1
		End If
		If TipFull3 <= 0 Then Bumper003tim.Enabled = 0
	End If
End Sub

'*****************
'Targets
'*****************

'*****************
'ball saved
'*****************
Dim SafeBall:SafeBall = 0
sub Target008_hit()
	PlaySound "fx_target"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	SafeBall = SafeBall OR 1	'Set SafeBall to the right being hit
	Select Case SafeBall
		Case 1	'I've already been hit...
			li049.state = 2	'Set the light to blinking
			Exit Sub	'and don't do anything more
		Case 3	'The left target was hit already
			li049.state = 1	'Set the light to be on
			balltosafty.enabled = false		'reset the timer, in case it's already on
			balltosafty.enabled = true
			EnableBallSaver 30
			SafeBall = 0
		Case Else	'EP- Should never get here, something's wrong... probably my coding
			'PlaySound "Knocker"
	End Select
end Sub

sub Target009_hit()
	PlaySound "fx_target"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	SafeBall = SafeBall OR 2	'Set SafeBall to the left being hit
	Select Case SafeBall
		Case 2	'I've already been hit...
			li049.state = 2	'Set the light to blinking
			Exit Sub	'and don't do anything more
		Case 3	'The right target was hit already
			li049.state = 1	'Set the light to be on
			balltosafty.enabled = false		'reset the timer, in case it's already on
			balltosafty.enabled = true
			EnableBallSaver 30
			SafeBall = 0
		Case Else	'EP- Should never get here, something's wrong... probably my coding
			Exit Sub
	End Select
end Sub

sub balltosafty_timer
	li049.state = 0
	balltosafty.enabled = 0
end sub

'*****************
'INSULT
'*****************
Dim in_, su_, lt_
Sub Target005_hit()	'IN target
	If Status = "Normal" AND VoodooSlot <> 31 Then
		li037.state = 1
		li038.state = 1
		in_ = "IN"
		PlayQuoteInsult()
		DMD CL(0, "" & in_ & su_ & lt_), CL(1, "SPELL INSULT"),"",0,0,0,1000,True,""
		CheckInsult()
	End If
	PlaySound "fx_target", 0, 0.2, Pan(ActiveBall) * 10
	TargetBonus = TargetBonus + 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	LSIns001.Play SeqCircleOutOn, 10
End Sub

Sub Target007_hit()
	If Status = "Normal" AND VoodooSlot <> 31 Then
		li039.state = 1
		li040.state = 1
		PlayQuoteInsult()
		su_ = "SU"
		DMD CL(0, "" & in_ & su_ & lt_), CL(1, "SPELL INSULT"),"",0,0,0,1000,True,""	
		CheckInsult()
	End If
	PlaySound "fx_target", 0, 0.2, Pan(ActiveBall) * 100
	TargetBonus = TargetBonus + 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	LSIns002.Play SeqCircleOutOn, 10
End Sub

Sub Target006_hit()
	if Status = "Normal" AND VoodooSlot <> 31 Then
		li041.state = 1
		li042.state = 1
		PlayQuoteInsult()
		lt_ = "LT"
		DMD CL(0, "" & in_ & su_ & lt_), CL(1, "SPELL INSULT"),"",0,0,0,1000,True,""
		CheckInsult()
	End If
	PlaySound "fx_target", 0, 0.2, Pan(ActiveBall) * 100
	TargetBonus = TargetBonus + 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	LSIns003.Play SeqCircleOutOn, 10
End Sub

Sub CheckInsult()
	if (in_ = "IN") And (su_ = "SU") And (lt_ = "LT") Then 
		if voodoounlocked = 0 Then
			playsound "Unlock"
		end if
		voodoounlocked = 1
		DMDFlush()
		DMD "", "", "dmdinsult", eNone, eNone, eNone, 1000, True, ""
		li047.state = 0
		li037.state = 2
		li038.state = 2
		li039.state = 2
		li040.state = 2
		li041.state = 2
		li042.state = 2
		If Status = "Normal" Then li011.state = 2
		Score(CurrentPlayer) = Score(CurrentPlayer) + (75000*PFMultiplier)
	end if
End Sub

'*****************
'monkey battles
'*****************
Dim Monkey3Hit:Monkey3Hit = 0
sub Target003_hit()		'Left Monkey
	'Updatemonkeycountr
	plank3Shaker()
	TargetBonus = TargetBonus + 1
	PlaySound "monkeyhit"
	'Play the angry animation on the monkey
	Monkey3Hit = 1
	'change the animation speed since being angry is faster... ?
	FrameNext3 = FrameRate3b
	'make the next frame to be played the first angry frame
	Monkey3Frame = 11
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	li003.state = 1
	CheckMonkeyBattle()
end sub

Dim Monkey2Hit:Monkey2Hit = 0
sub Target002_hit()		'Right Monkey
'Updatemonkeycountr
	plank2Shaker()
	TargetBonus = TargetBonus + 1
	PlaySound "monkeyhit"
	Monkey2Hit = 1
	FrameNext2 = FrameRate2b
	Monkey2Frame = 11
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	li005.state = 1
	CheckMonkeyBattle
end sub

Dim Monkey1Hit:Monkey1Hit = 0
sub Target001_hit()		'Center Monkey
'Updatemonkeycountr
	plank1Shaker()
	TargetBonus = TargetBonus + 1
	PlaySound "monkeyhit"
	Monkey1Hit = 1
	FrameNext1 = FrameRate1b
	Monkey1Frame = 11
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	li004.state = 1
	CheckMonkeyBattle
end sub

Sub CheckMonkeyBattle()
	If(li003.State = 1) And(li004.State = 1) And(li005.State = 1) Then
	'Playsound "po_fanfare5"
	AddScore 25000
	li004.State=0
	li005.state=0
	li003.state=0
	monkeybattle = monkeybattle + 1
	Updatemonkeycountr
	vpmtimer.addtimer 250, "startB2S(3) '"
	End If
End Sub

'***********plank shakers*************
'Dim plank1Shake
Dim plank2Shake
Dim plank3Shake

Sub plank1Shaker()
'	 plank1Shake = 6
	plank1Timer.Enabled = True
End Sub

'A number between 1 and 360 to calculate Sin on
Dim PlankShake1:PlankShake1 = 1
'How far to move the plank; bigger numbers will increase how far away from the center it moves, smaller numbers means smaller movement
'We could get the velocity of the ball when it hits the plank to dynamically change this.  Harder hits = bigger shakes (and longer since it has to move farther)
Dim PlankShakeDist1:PlankShakeDist1 = 12
'How fast the plank shakes; bigger numbers will make it vibrate, smaller numbers will make it move in slow motion
Dim PlankShakeSpeed1:PlankShakeSpeed1 = 4
'How stiff the plank is; bigger numbers will make it wobble less as if it were connect to stiff rod, smaller numbers make it wobble longer like it was on a loose spring
Dim PlankStiff1:PlankStiff1 = 0.5
Sub plank1Timer_Timer()
	me.Interval = 1
	'Use the same math as on the barrells to shake the planks
	plank1.TransZ = dSin(PlankShake1)*PlankShakeDist1
	'Increase the number the math does work on at a rate we have control over
	PlankShake1 = PlankShake1 + PlankShakeSpeed1
	'We don't want this number to increment indefinitely since 361 is the same as 1 when it comes to calculating Sin
	If PlankShake1 > 360 Then
		PlankShake1 = 1
		'Decrease the distance it moves from center each time it moves left and right so the next time it moves left and right, it doesn't move as far
		PlankShakeDist1 = PlankShakeDist1 - PlankStiff1
	End If
	'If it's shaken back and forth the specified number of times (PlankShakeDist1), then we're done
	If PlankShakeDist1 <= 0 Then
		'Turn off the animation
		me.Enabled = 0
		'Reset the shake amount for next time
		PlankShakeDist1 = 12
	End If
End Sub

Sub plank2Shaker()
	plank2Shake = 6
	plank2Timer.Enabled = True
End Sub

Dim PlankShake2:PlankShake2 = 1
Dim PlankShakeDist2:PlankShakeDist2 = 12
Dim PlankShakeSpeed2:PlankShakeSpeed2 = 4
Dim PlankStiff2:PlankStiff2 = 0.5
Sub plank2Timer_Timer()
	me.Interval = 1
	plank2.TransZ = dSin(PlankShake2)*PlankShakeDist2
	PlankShake2 = PlankShake2 + PlankShakeSpeed2
	If PlankShake2 > 360 Then
		PlankShake2 = 1
		PlankShakeDist2 = PlankShakeDist2 - PlankStiff2
	End If
	If PlankShakeDist2 <= 0 Then
		me.Enabled = 0
		PlankShakeDist2 = 12
	End If
End Sub

Sub plank3Shaker()
	plank3Shake = 6
	plank3Timer.Enabled = True
End Sub

Dim PlankShake3:PlankShake3 = 1
Dim PlankShakeDist3:PlankShakeDist3 = 12
Dim PlankShakeSpeed3:PlankShakeSpeed3 = 4
Dim PlankStiff3:PlankStiff3 = 0.5
Sub plank3Timer_Timer()
	me.Interval = 1
	plank3.TransZ = dSin(PlankShake3)*PlankShakeDist3
	PlankShake3 = PlankShake3 + PlankShakeSpeed3
	If PlankShake3 > 360 Then
		PlankShake3 = 1
		PlankShakeDist3 = PlankShakeDist3 - PlankStiff3
	End If
	If PlankShakeDist3 <= 0 Then
		me.Enabled = 0
		PlankShakeDist3 = 12
	End If
End Sub

'*****************
'Gates
'*****************
sub Gate_Hit()
	Playsound "gate1"
	LightSeq002.StopPlay
	FlashForMs Flasher020, 1000, 50, 0
	FlashForMs Flasher019, 1000, 50, 0
	startB2S(3)
	If PFMultiplier > 1 then exit sub
	LightSeq001.Play SeqRightOn, 25, 1000
End Sub

sub Gate003_Hit()
	Playsound "dock"
End Sub

sub Gate001_Hit()
	Playsound "gate004"
	movegraves
End Sub

sub movegraves
	Playsound "monkeytargup"
	grave001.ObjRotx = 20
	grave002.ObjRotx = 20
	grave003.ObjRotx = 20
	vpmTimer.AddTimer 500, "gravesnormal '" 
end sub

sub gravesnormal
	Playsound "monkeytargup"
	grave001.ObjRotx = 0
	grave002.ObjRotx = 0
	grave003.ObjRotx = 0
end sub


'*****************
'Kickers
'*****************
Dim dBall

'*****************Lechuck**************************
Dim LeChuckHits:LeChuckHits = 0

sub Kicker001_hit()
	If LeChuckHits >= 3 AND Status = "Normal"  AND LeChuckSlot <> 255 Then
		Set dBall = ActiveBall
		MyTroughLeChuckAdd(dBall)
		StartLechuckItemsSlotmachine()
		Exit Sub
	End If
	If Status = "Normal" AND LeChuckSlot <> 255 Then
		LeChuckHits = LeChuckHits + 1
		Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	End If
	ObjLevel(8) = 1
	FlasherFlash8_Timer
	PlayLechuckQuote()
	Set dBall = ActiveBall
	MyTroughLeChuckAdd(dBall)
	StartLeChuckSlotMachineLechuck()
end sub

'***************** Vulcano**************************
sub Kicker004_hit()
	FlashForMs Flasher019, 1000, 50, 0
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	vpmTimer.addTimer 1000, "vulcanokick '"
end sub

sub vulcanokick()
	Playsound "LavaBall"
	Kicker004.Kick 0,35,1.56
end sub

'*****************Bar kicker**************************
Dim eBall

sub Kicker006_hit()
	playsound "balldropy"
	FlashForMs Flasher002, 1000, 50, 0
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	Set eBall = ActiveBall
	Me.TimerEnabled = 1
end sub

Sub Kicker006_Timer
	if (barunlocked = 0) or (Status <> "Normal") or (bMultiBallMode = true) Then
		If eBall.Z > 0 Then
			eBall.Z = eBall.Z - 5
			Exit Sub
		End If
		MyTroughLRAdd(eBall)
		Me.DestroyBall
		startB2S(4)
		LowerTarget013()
		LrKickout 1000, 290, 7, 0
	Else
		StartBarSlotmachine()
	End If
	Me.TimerEnabled = 0
End Sub

'*****************Voodoo kicker**************************
Dim cBall

sub Kicker005_hit()
	FlashForMs Flasher001, 1000, 50, 0
'	vpmtimer.addtimer 250, "startB2S(3) '"
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	'Animate the ball going down
	Set cBall = ActiveBall
	Me.TimerEnabled = 1
end sub

Sub Kicker005_Timer
	'If voodoo is locked, you're in a quest, or in a multiball mode, then kickout from the right
	if (voodoounlocked = 0) or (Status <> "Normal") or (bMultiBallMode = true) Then
		'Lower the ball
		If cBall.Z > 0 Then
			cBall.Z = cBall.Z - 5
			Exit Sub
		End If
		MyTroughLRAdd(cBall)
		'When it's all the way down
		Me.DestroyBall
		startB2S(4)
		LowerTarget013()
		LRKickout 1000, 290, 7, 0
	'Otherwise, Do something based on the mode we're in
	Else
		Select Case Status
			Case "Normal"
				StartVoodooSlotmachine()
			Case Else
				LRKickout 1000, 290, 7, 0
		End Select
	end if
	Me.TimerEnabled = 0
End Sub

Sub FromVoodooToLowerRight()
	PlaySound "knocker"
End Sub

'We have to do it this way so that newly entering balls don't trigger ball creating immediately; ball creation should only happen 1 second after a ball is kicked out
Kicker002.UserValue = 0
Sub LRKickout(wait, tAngle, tForce, tZ)
	'If there are balls in the trough
	If UBound(MyTroughLR) >= 0 Then
		'If the kicker is ready to kick...
		If Kicker002.UserValue = 0 Then
			'Make the kicker not ready to kick
			Kicker002.userValue = 1
			FlashForMs Flasher009, 1000, 50, 0
			'And kick the ball after the desire wait
			vpmTimer.AddTimer wait, "LRKickout2 " & tAngle & ", " & tForce & ", " & tz & " '"
		End If
	End If
End Sub

sub LRKickout2(tAngle, tForce, tZ)
	'Create the ball
	Kicker002.CreateSizedBall(BallSize/2).ID = MyTroughLR(0)
	'Kick it
	Kicker002.Kick tAngle, tForce, tZ
	PlaySound "fx_popper"
	'Remove the ball from the trough
	MyTroughLRRemove()
	'Show that the kicker is ready
	Kicker002.userValue = 0
	'Call LRKickout in case there are more balls in the trough
	LRKickout 1000, 290, 7, 0
	'If the trough is empty, close the door, otherwise keep it open
	If UBound(MyTroughLR) < 0 Then vpmTimer.Addtimer 500, "ResetTarget013 '"
End Sub

Sub BarKickout(wait, tAngle, tForce, tZ)
	If UBound(MyTroughTreasure) >= 0 Then
		BarKickout2 tAngle, tForce, tz
	End If
End Sub

sub BarKickout2(tAngle, tForce, tZ)
	Kicker006.CreateSizedBall(BallSize/2).ID = MyTroughTreasure(0)
	Kicker006.Kick tAngle, tForce, tZ
	Set rBall = Nothing
	PlaySound "fx_popper"
	MyTroughTreasureRemove()
	BarKickout 1000, 290, 7, 0
End Sub

Sub LeChuckKickout(wait, tAngle, tForce, tZ)
	If UBound(MyTroughLeChuck) >= 0 Then
		vpmTimer.AddTimer wait, "LeChuckKickout2 " & tAngle & ", " & tForce & ", " & tz & " '"
	End If
End Sub

sub LeChuckKickout2(tAngle, tForce, tZ)
	Set dBall = Nothing
	Kicker001.DestroyBall
	Kicker003.CreateSizedBall(BallSize/2).ID = MyTroughLeChuck(0)
	Kicker003.Kick tAngle, tForce, tZ
	PlaySound "fx_popper"
	MyTroughLeChuckRemove()
	LeChuckKickout 1000, 290, 7, 0
End Sub

Sub TreasureKickout(wait, tAngle, tForce, tZ)
	If UBound(MyTroughTreasure) >= 0 Then
		vpmTimer.AddTimer wait, "TreasureKickout2 " & tAngle & ", " & tForce & ", " & tz & " '"
	End If
End Sub

sub TreasureKickout2(tAngle, tForce, tZ)
	Kicker007.CreateSizedBall(BallSize/2).ID = MyTroughTreasure(0)
	Kicker007.Kick tAngle, tForce, tZ
	PlaySound "fx_popper"
	MyTroughTreasureRemove()
	TreasureKickout 1000, 290, 7, 0
End Sub

Sub MyTroughLRAdd(tBall)
	'Increase the size of the array
	ReDim Preserve MyTroughLR(UBound(MyTroughLR) + 1)
	'Set the end of the array equal to the ball id that just entered
	MyTroughLR(UBound(MyTroughLR)) = tBall.ID
	'Free up the object that was the ball
	Set tBall = Nothing
End Sub

Sub MyTroughTreasureAdd(tBall)
	ReDim Preserve MyTroughTreasure(UBound(MyTroughTreasure) + 1)
	MyTroughTreasure(Ubound(MyTroughTreasure)) = tBall.ID
	Set tBall = Nothing
End Sub

Sub MyTroughLeChuckAdd(tBall)
	ReDim Preserve MyTroughLeChuck(UBound(MyTroughLeChuck) + 1)
	MyTroughLeChuck(Ubound(MyTroughLeChuck)) = tBall.ID
	Set tBall = Nothing
End Sub

Sub MyTroughLRRemove()
	Dim x
	'Clear the ball that just got kicked
	'MyTroughLR(0) = 0
	'For every other ball in array, "move" each ball over
	For X = 1 to UBound(MyTroughLR)
		MyTroughLR(X - 1) = MyTroughLR(X)
	Next
	'And shrink the array
	ReDim Preserve MyTroughLR(UBound(MyTroughLR)-1)
End Sub

Sub MyTroughTreasureRemove()
	Dim X
	MyTroughTreasure(0) = 0
	For X = 1 to UBound(MyTroughTreasure)
		MyTroughTreasure(X - 1) = MyTroughTreasure(X)
	Next
	ReDim Preserve MyTroughTreasure(UBound(MyTroughTreasure) - 1)
End Sub

Sub MyTroughLeChuckRemove()
	Dim X
	MyTroughLeChuck(0) = 0
	For X = 1 to UBound(MyTroughLeChuck)
		MyTroughLeChuck(X - 1) = MyTroughLeChuck(X)
	Next
	ReDim Preserve MyTroughLeChuck(UBound(MyTroughLeChuck) - 1)
End Sub

'*****************right lower kicker/drop target**************************
Dim fBall
sub Kicker002_hit()
	FlashForMs Flasher009, 1000, 50, 0
	Score(CurrentPlayer) = Score(CurrentPlayer) + (5000*PFMultiplier)
	Set fBall = ActiveBall
	Kicker002.TimerEnabled = 1
end sub

Sub Kicker002_Timer()
	If fBall.Z > 0 Then
		fBall.Z = fBall.Z - 5
		Exit Sub
	End If
	MyTroughLRAdd(fBall)
	Me.DestroyBall
	startB2S(4)
	LowerTarget013()
	LRKickout 100, 290, 7, 0
	Me.Enabled = 0
	Me.TimerEnabled = 0
	vpmtimer.AddTimer 2000, "Kicker002Enable '"
End Sub
	
Sub Target013_Hit()
	playsound "dropopen"
	If Tilted Then Exit Sub
	Score(CurrentPlayer) = Score(CurrentPlayer) + (1000*PFMultiplier)
	TargetBonus = TargetBonus + 1
	vpmtimer.addtimer 1000, "ResetTarget013 '"
End Sub

sub ResetTarget013()
	If Target013.IsDropped = 1 Then	PlaySound "dropclose"
	Target013.IsDropped = 0
end sub

Sub LowerTarget013()
	If Target013.IsDropped = 0 Then PlaySound "dropopen"
	Target013.IsDropped = 1
End Sub

Sub KiGrog_Hit()
	'Do stuff to start Grog modes
End Sub

'*****************
'lava animation
'*****************
Dim Fire1Pos,Flames
Flames = Array("lava000", "lava001", "lava002", "lava003", "lava004", "lava005", "lava006", "lava007", "lava008", "lava009", "lava010", "lava011", "lava012", "lava013", "lava014", "lava015", "lava016",_														   
"lava017", "lava018", "lava019", "lava020", "lava021", "lava022", "lava023", "lava024", "lava025", "lava026", "lava027", "lava028", "lava029", "lava030", "lava031", "lava032", "lava033",_												 
"lava034", "lava035", "lava036", "lava037", "lava038", "lava039", "lava040", "lava041", "lava042", "lava043", "lava044", "lava045", "lava046", "lava047", "lava048", "lava049", "lava050",_															  
"lava051", "lava052", "lava053", "lava054", "lava055", "lava056", "lava057", "lava058", "lava059", "lava060", "lava061", "lava062", "lava063", "lava064", "lava065", "lava066", "lava067",_														   
"lava068", "lava069", "lava070", "lava071", "lava072", "lava073", "lava074", "lava075", "lava076", "lava077", "lava078", "lava079", "lava080", "lava081", "lava082", "lava083", "lava084",_															  
"lava085", "lava086", "lava087", "lava088", "lava089", "lava090", "lava091", "lava092", "lava093", "lava094", "lava095", "lava096", "lava097", "lava098", "lava099", "lava100", "lava101")

Sub StartFire
	Fire1Pos = 0
	FireTimer.Enabled = 1
End Sub

Sub FireTimer_Timer
	lava.ImageA = Flames(Fire1Pos)
	Fire1Pos = (Fire1Pos + 1) MOD 102
End Sub

Dim Fire2Pos,Flames2
Flames2 = Array("magma00", "magma01", "magma02", "magma03", "magma04", "magma05", "magma06", "magma07", "magma08", "magma09", "magma10", "magma11", "magma12", "magma13", "magma14", "magma15", "magma16",_
"magma17", "magma18", "magma19", "magma20", "magma21", "magma22", "magma23", "magma24", "magma25", "magma26", "magma27", "magma28", "magma29", "magma30", "magma31")

Sub StartFire2
	Fire2Pos = 0
	FireTimer2.Enabled = 1
End Sub

Sub FireTimer2_Timer
	magma.ImageA = Flames2(Fire2Pos)
	Fire2Pos = (Fire2Pos + 1) MOD 32
End Sub

'*****************
'Dail a pirate
'*****************
Dim WheelSpeed : WheelSpeed = 0.10
Dim SlowSpeed : SlowSpeed = 0.0005
Sub spinningwheel_Timer
	Select Case InAttract
		Case 1
			Me.Interval = 1
			Wheel1.Rotz = (Wheel1.Rotz + WheelSpeed)
			WheelSpeed = WheelSpeed - SlowSpeed
			If WheelSpeed <= 0.10 Then WheelSpeed = 0.10
			If Wheel1.Rotz >= 360 Then Wheel1.Rotz = 1
		Case 0
			Me.Interval = 1
			Wheel1.Rotz = (Wheel1.Rotz + WheelSpeed)
			If Wheel1.Rotz >= 360 Then Wheel1.Rotz = 1
			WheelSpeed = WheelSpeed - SlowSpeed
			If WheelSpeed <= 0.005 Then
				Me.Enabled = False
				Select Case True
					Case (Wheel1.Rotz <= 36 AND Wheel1.Rotz > 12)
						Wheel1.Rotz = 24
					Case (Wheel1.Rotz <= 60 AND Wheel1.Rotz > 36)
						Wheel1.Rotz = 48
					Case (Wheel1.Rotz <= 84 AND Wheel1.Rotz > 60)
						Wheel1.Rotz = 72
					Case (Wheel1.Rotz <= 108 AND Wheel1.Rotz > 84)
						Wheel1.Rotz = 98
					Case (Wheel1.Rotz <= 132 AND Wheel1.Rotz > 108)
						Wheel1.Rotz = 128
					Case (Wheel1.Rotz <= 156 AND Wheel1.Rotz > 132)
						Wheel1.Rotz = 146
					Case (Wheel1.Rotz <= 180 AND Wheel1.Rotz > 156)
						Wheel1.Rotz = 168
					Case (Wheel1.Rotz <= 204 AND Wheel1.Rotz > 180)
						Wheel1.Rotz = 192
					Case (Wheel1.Rotz <= 228 AND Wheel1.Rotz > 204)
						Wheel1.Rotz = 216
					Case (Wheel1.Rotz <= 252 AND Wheel1.Rotz > 228)
						Wheel1.Rotz = 240
					Case (Wheel1.Rotz <= 276 AND Wheel1.Rotz > 252)
						Wheel1.Rotz = 264
					Case (Wheel1.Rotz <= 300 AND Wheel1.Rotz > 276)
						Wheel1.Rotz = 288
					Case (Wheel1.Rotz <= 324 AND Wheel1.Rotz > 300)
						Wheel1.Rotz = 312
					Case (Wheel1.Rotz <= 348 AND Wheel1.Rotz > 324)
						Wheel1.Rotz = 336
					Case (Wheel1.Rotz <= 12 AND Wheel1.Rotz > 348)
						Wheel1.Rotz = 0
				End Select
			End If
	End Select
End Sub

'*****************
'smoke animation
'*****************
Dim CanonSmokeAnim:CanonSmokeAnim = Array("smoke1", "smoke2", "smoke3", "smoke4", "smoke5", "smoke6", "smoke6", "smoke7", "smoke8", "smoke9", "smoke10", "smoke11", "smoke12", "smoke13", "smoke14", "smoke15""smoke16")
Dim CanonSmokeAnimKey:CanonSmokeAnimKey = 0
Dim SmokePositionYOrig:SmokePositionYOrig = canonsmoke.y
Dim FadeAmount:FadeAmount = 1/UBound(CanonSmokeAnim)
Sub smokeytimer_Timer()
	canonsmoke.visible = 1
	canonsmoke.ImageA = CanonSmokeAnim(CanonSmokeAnimKey)
	CanonSmokeAnimKey = CanonSmokeAnimKey + 1
	canonsmoke.height = canonsmoke.height + 5
	canonsmoke.y = canonsmoke.y - 2
	canonsmoke.IntensityScale = canonsmoke.IntensityScale - FadeAmount
	If CanonSmokeAnimKey > UBound(CanonSmokeAnim) Then
		ResetSmoke()
	End If
End Sub

sub resetsmoke()
	smokeytimer.enabled = False
	canonsmoke.visible = 0
	CanonSmokeAnimKey = 0
	canonsmoke.height = 200
	canonsmoke.y = SmokePositionYOrig
	canonsmoke.IntensityScale = 1
end sub

'*****************
'beard and palmtrees
'*****************
Dim Sway

sub beardtreetimer_Timer()
'Palm Trees
	tree001.RotY = dSin(Sway)*2
	tree002.RotY = tree001.RotY
	tree003.RotY = tree001.RotY
	tree004.RotY = tree001.RotY
	tree005.RotY = tree001.RotY
	tree006.RotY = tree001.RotY
	tree007.RotY = tree001.RotY
	tree008.RotY = tree001.RotY
	Leaves001.RotY = tree001.RotY
	Leaves002.RotY = tree001.RotY
	Leaves003.RotY = tree001.RotY
	Leaves004.RotY = tree001.RotY
	Leaves005.RotY = tree001.RotY
	Leaves006.RotY = tree001.RotY
	Leaves007.RotY = tree001.RotY
	Leaves008.RotY = tree001.RotY
	Sway = Sway + 0.05
	If Sway > 360 Then Sway = 1
	'******
	'Le chuck's Beard matches the gate
	Beard.RotX = -Gate002.CurrentAngle
	'******
end sub

'*****************
'* Maths
'*****************
Dim Pi
Pi = Round(4 * Atn(1), 6)
Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
	if ABS(dSin) < 0.000001 Then dSin = 0
	if ABS(dSin) > 0.999999 Then dSin = 1 * sgn(dSin)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
	if ABS(dCos) < 0.000001 Then dCos = 0
	if ABS(dCos) > 0.999999 Then dCos = 1 * sgn(dCos)
End Function

'*****************
' pirateship animation
'*****************

Dim pirateshipDir, SinkPirateShip, SinkSpeed, ShipSpeed, ShipTip, ShipAngle, ShipAngleSpeed, ShipHeight
Dim ShipOrigX, ShipOrigY, ShipOrigZ
ShipSpeed = 6
pirateshipDir = ShipSpeed 'this is the direction, if + goes up, if - goes down
SinkPirateShip = 0
SinkSpeed = 0.5
ShipTip = 15
ShipAngle = 180
ShipAngleSpeed = 2.5
ShipHeight = 25
ShipOrigX = PirateShip.X
ShipOrigY = PirateShip.Y
ShipOrigZ = PirateShip.Z

Sub PirateShipTimer_Timer
	Select Case SinkPirateShip
		'Ship is moving back and forth
		Case 0
			PirateShip.Y = ShipOrigY + dCos(ShipAngle*0.25+135) * 300 + 300
			PirateShip.Z = dCos(ShipAngle) * ShipHeight + ShipHeight + 55
			ShipAngle = ShipAngle + ShipAngleSpeed
			PirateShip.RotZ = dSin(ShipAngle) * ShipTip
			If ShipAngle > 1440 Then ShipAngle = ShipAngle - 1440
		Case 1
			'Ship got hit, so start the sinking animation
			Select Case true
				Case pirateship.RotZ < 90
					pirateship.RotZ = pirateship.RotZ + SinkSpeed * 1.5
				Case pirateship.RotZ >= 90
					Pirateship.Z = Pirateship.Z - SinkSpeed * 1.5
					If pirateship.Z <= -250 Then
						SinkPirateShip = 2
						pirateship.Y = 1167.221
						pirateship.RotZ = 0
					End If
			End Select
		Case 2
			pirateship.z = pirateship.z + SinkSpeed * 1.5
			If pirateship.z >= 55 Then
				SinkPirateShip = 0
				ShipAngle = 180
				me.enabled = 0
			End If
	End Select
	FlShipShadow.X = PirateShip.X
	FlShipShadow.Y = PirateShip.Y
End Sub

'**************
' SlotMachine le chuck items
'**************
Dim SlotAward3, SlotValue3

SlotAward3 = Array("itm1", "itm2", "itm3", "itm4", "itm5", "itm6", "itm7", "itm8" )

Sub StartLechuckItemsSlotmachine()
	startB2S(9)
	Dim i
	DMDFlush()
	For i = 0 to 7
		DMD "", "", SlotAward3(i), eNone, eNone, eNone, 50, False, "fx_spinner"
	Next
	vpmtimer.AddTimer 500, "GiveSlotAwardItemLeChuck '"
End Sub

Dim LeChuckSlot:LeChuckSlot = 0
Sub GiveSlotAwardItemLeChuck()
	'LightSeq001.StopPlay
	DMDFlush()
	Randomize()
	SlotValue3 = INT(RND * 8 + 1)
	DMD "", "", SlotAward3(SlotValue3 - 1), eNone, eNone, eNone, 1000, True, ""
	Select Case SlotValue3
		Case 3
			SlotValue3 = 4
		Case 4
			SlotValue3 = 8
		Case 5
			SlotValue3 = 16
		Case 6
			SlotValue3 = 32
		Case 7
			SlotValue3 = 64
		Case 8
			SlotValue3 = 128
	End Select
	'Check if this mode has already been played
	Do While (SlotValue3 AND LeChuckSlot) > 0	'We've already done this mode So keep randomly picking one until we get a mode we haven't done yet
		SlotValue3 = INT(RND * 8 + 1)
		DMDFlush()
		DMD "", "", SlotAward3(SlotValue3-1), eNone, eNone, eNone, 500, True, ""
		Select Case SlotValue3
			Case 3
				SlotValue3 = 4
			Case 4
				SlotValue3 = 8
			Case 5
				SlotValue3 = 16
			Case 6
				SlotValue3 = 32
			Case 7
				SlotValue3 = 64
			Case 8
				SlotValue3 = 128
		End Select
	Loop
	Select Case SlotValue3
		Case 1:Lechuck_item1()		'Bone
		Case 2:Lechuck_item2()		'Book
		Case 4:Lechuck_item3()		'Skull
		Case 8:Lechuck_item4()		'Sword
		Case 16:Lechuck_item5()		'Mok
		Case 32:Lechuck_item6()		'Map
		Case 64:Lechuck_item7()		'Compass
		Case 128:Lechuck_item8()	'Keys
	End Select
End Sub

'********Item 1***************
sub Lechuck_item1
	StopSong()
	PlaySong "lechuckitems" 
	mode1TimerCount = 60
	mode1timer.Enabled = 1
	itemrotytimer.Enabled = 1
	titem001.enabled = 1
	bone.Visible = 1
	li014.state = 2
	LeChuckKickout 1000, 270, 7, 0
	status = "Bone"
end Sub

sub titem001_hit()
	DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
	Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
	li014.state = 1
	bone.Visible = 0
	playsound "won"
	StopmodeEndofBall()
	titem001.enabled = 0
	AddMultiball 1
	Status = "Normal"
	ChangeSong()
	LeChuckSlot = LeChuckSlot OR 1
	checkitemscomplete()
End sub

'********Item 2***************
sub Lechuck_item2
	StopSong()
	PlaySong "lechuckitems" 
	mode1TimerCount = 60
	mode1timer.Enabled = 1
	itemrotztimer.Enabled = 1
	titem002.enabled = 1
	book.Visible = 1
	li015.state = 2
	LeChuckKickout 1000, 270, 7, 0
	Status = "Book"
end Sub

sub titem002_hit()
	DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
	Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
	li015.state = 1
	book.Visible = 0
	playsound "won"
	StopmodeEndofBall()
	titem002.enabled = 0
	AddMultiball 1
	ChangeSong()
	LeChuckSlot = LeChuckSlot OR 2
	checkitemscomplete()
	Status = "Normal"
End sub

'********Item 3***************
sub Lechuck_item3
	StopSong()
	PlaySong "lechuckitems" 
	mode1TimerCount = 60
	mode1timer.Enabled = 1
	itemrotytimer.Enabled = 1
	titem003.enabled = 1
	skully.Visible = 1
	li016.state = 2
	LeChuckKickout 1000, 270, 7, 0
	Status = "Skully"
end Sub

sub titem003_hit()
	DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
	Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
	li016.state = 1
	skully.Visible = 0
	playsound "won"
	StopmodeEndofBall()
	titem003.enabled = 0
	AddMultiball 1
	Status = "Normal"
	ChangeSong()
	LeChuckSlot = LeChuckSlot OR 4
	checkitemscomplete()
End sub

'********Item 4***************
sub Lechuck_item4
	StopSong()
	PlaySong "lechuckitems" 
	mode1TimerCount = 60
	mode1timer.Enabled = 1
	itemrotztimer.Enabled = 1
	titem004.enabled = 1
	sword001.Visible = 1
	li017.state = 2
	LeChuckKickout 1000, 270, 7, 0
	Status = "Sword"
end Sub

sub titem004_hit()
	DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
	Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
	li017.state = 1
	sword001.Visible = 0
	playsound "won"
	StopmodeEndofBall()
	titem004.enabled = 0
	AddMultiball 1
	Status = "Normal"
	ChangeSong()
	LeChuckSlot = LeChuckSlot OR 8
	checkitemscomplete()
End sub

'********Item 5***************
sub Lechuck_item5
	StopSong()
	PlaySong "lechuckitems" 
	mode1TimerCount = 60
	mode1timer.Enabled = 1
	itemrotytimer.Enabled = 1
	titem005.enabled = 1
	mok.Visible = 1
	li018.state = 2
	LeChuckKickout 1000, 270, 7, 0
	Status = "Mok"
end Sub

sub titem005_hit()
	DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
	Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
	li018.state = 1
	mok.Visible = 0
	playsound "won"
	StopmodeEndofBall
	titem005.enabled = 0
	AddMultiball 1
	Status = "Normal"
	ChangeSong()
	LeChuckSlot = LeChuckSlot OR 16
	checkitemscomplete()
End sub

'********Item 6***************
sub Lechuck_item6
	StopSong()
	PlaySong "lechuckitems" 
	mode1TimerCount = 60
	mode1timer.Enabled = 1
	itemrotytimer.Enabled = 1
	titem006.enabled = 1
	Map.Visible = 1
	li019.state = 2
	LeChuckKickout 1000, 270, 7, 0
	Status = "Map"
end Sub

sub titem006_hit()
	DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
	Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
	li019.state = 1
	Map.Visible = 0
	playsound "won"
	StopmodeEndofBall()
	titem006.enabled = 0
	AddMultiball 1
	Status = "Normal"
	ChangeSong()
	LeChuckSlot = LeChuckSlot OR 32
	checkitemscomplete()
End sub

'********Item 7***************
sub Lechuck_item7
	StopSong()
	PlaySong "lechuckitems" 
	mode1TimerCount = 60
	mode1timer.Enabled = 1
	itemrotytimer.Enabled = 1
	titem007.enabled = 1
	compas.Visible = 1
	li020.state = 2
	LeChuckKickout 1000, 270, 7, 0
	Status = "Compass"
end Sub

sub titem007_hit()
	DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
	Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
	li020.state = 1
	compas.Visible = 0
	playsound "won"
	StopmodeEndofBall()
	titem007.enabled = 0
	AddMultiball 1
	Status = "Normal"
	ChangeSong()
	LeChuckSlot = LeChuckSlot OR 64
	checkitemscomplete()
End sub

'********Item 8***************
sub Lechuck_item8
	StopSong()
	PlaySong "lechuckitems" 
	mode1TimerCount = 60
	mode1timer.Enabled = 1
	itemrotytimer.Enabled = 1
	titem008.enabled = 1
	keysitem.Visible = 1
	li021.state = 2
	LeChuckKickout 1000, 270, 7, 0
	Status = "Keys"
end Sub

sub titem008_hit()
	DMD "", "", "200k", eNone, eNone, eNone, 1000, True, ""
	Score(CurrentPlayer) = Score(CurrentPlayer) + (200000*PFMultiplier)
	li021.state = 1
	keysitem.Visible = 0
	playsound "won"
	StopmodeEndofBall()
	titem008.enabled = 0
	AddMultiball 1
	Status = "Normal"
	ChangeSong()
	LeChuckSlot = LeChuckSlot OR 128
	checkitemscomplete()
End sub

sub checkitemscomplete()
	If LeChuckSlot = 255 Then
		DMD "", "", "dmditmcomplete", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (1000000*PFMultiplier)
	end if
end sub

'**************
' SlotMachine le chuck
'**************

Dim SlotAward, SlotValue
SlotAward = Array("lc2", "lc1")
Sub StartLeChuckSlotMachineLechuck()
	startB2S(9)
	Dim i
	DMDFlush
	For i = 0 to 1
		DMD "", "", SlotAward(i), eNone, eNone, eNone, 100, False, ""
	Next
	vpmtimer.AddTimer 2000, "GiveSlotAwardLeChuck '"
End Sub

Sub GiveSlotAwardLeChuck()
	DMDFlush()
	Randomize()
	SlotValue = INT(RND * 2)
	DMD "", "", SlotAward(SlotValue), eNone, eNone, eNone, 500, True, ""
	Select Case SlotValue
		Case 0:LeChuckKickout 20, 270, 7, 0
		Case 1:LeChuckKickout 20, 90, 7, 0
	End Select
End Sub

'**************
' SlotMachine Bar
'**************

Dim SlotAward1, SlotValue1

SlotAward1 = Array("b1", "b2", "b3", "b4", "b5")

Sub StartBarSlotmachine()
	startB2S(8)
	playsound "bartalk"
	Dim i
	DMDFlush
	For i = 0 to 4
		DMD "", "", SlotAward1(i), eNone, eNone, eNone, 50, False, "fx_spinner"
	Next
	vpmtimer.AddTimer 2000, "GiveSlotAwardBar '"
	li007.state = 0
	li011.state = 0
End Sub

Dim BarSlot:BarSlot = 0
Sub GiveSlotAwardBar()
	DMDFlush()
	Randomize()
	SlotValue1 = INT(RND * 5) + 1
	DMD "", "", SlotAward1(SlotValue1 - 1), eNone, eNone, eNone, 500, True, ""
	Select Case SlotValue1
		Case 1
			SlotValue1 = 1	'Treasure
		Case 2
			SlotValue1 = 2	'Spitting
		Case 3
			SlotValue1 = 4	'Bar Fight
		Case 4
			SlotValue1 = 8	'Coin Frenzy
		Case 5
			SlotValue1 = 16	'Duels
	End Select
	Do while (SlotValue1 AND BarSlot) > 0
		SlotValue1 = Int(RND * 5 + 1)
		DMDFlush()
		DMD "", "", SlotAward1(SlotValue1-1), eNone, eNone, eNone, 500, True, ""
		Select Case SlotValue1
			Case 1
				SlotValue1 = 1	'Treasure
			Case 2
				SlotValue1 = 2	'Spitting
			Case 3
				SlotValue1 = 4	'Bar Fight
			Case 4
				SlotValue1 = 8	'Coin Frenzy
			Case 5
				SlotValue1 = 16	'Duels
		End Select
	Loop
	Select Case SlotValue1
		Case 1
			BSTreasure.AddBall Kicker006
			Set eBall = Nothing
			vpmTimer.AddTimer 1000, "Spitting '"		'Spitting
		Case 2
			'BSBarToBar.AddBall Kicker006
			vpmTimer.AddTimer 1000, "Treasure '"		'Treasure
		Case 4
			BSBarToBar.AddBall Kicker006
			Set eBall = Nothing
			vpmTimer.AddTimer 1000, "BarFight '"		'BarFight
		Case 8
			BSBarToBar.AddBall Kicker006
			Set eBall = Nothing
			vpmTimer.AddTimer 1000, "CoinFrenzy '"		'CoinFrenzy
		Case 16
			BSBarToBar.AddBall Kicker006
			Set eBall = Nothing
			vpmTimer.AddTimer 1000, "Duels '"			'Duels
	End Select
	BarSlot = BarSlot OR SlotValue1
	If BarSlot = 31 Then
		BarUnlocked = 0
		li048.state = 1
		li007.state = 0
	End If
End Sub

'********bar pitting mode***************

sub Spitting()
	li032.state = 2
	li032.state = 1
	BSBarToBar.ExitSol_On
	Status = "Spitting"
	Status = "Normal"
	If BarUnlocked = 1 Then li007.state = 2
	If VoodooUnlocked = 1 Then li011.state = 2
end sub

'********bar treasure hunt mode***************
dim rBall
dim BallInHole4
Dim SchatkistShake

sub Treasure()
	bringChestdown()
	StopSong()
	playsound "dig"
	PlaySong "barquest2" 
	vpmtimer.addtimer 1050, "kickbonus '"
	li033.state = 2
	Status = "Treasure"
end sub

sub bringChestdown()
	duuus()
	ChestDir = 1
	ChestdownTimer.enabled = 1
end sub

Sub kickbonus()
	LowerFlippersActive = True
	bonustime = 1
	mode2Timercount = 7
	mode2timer.Enabled = 10
end sub

sub Kicker007_hit()
	if bonustime = 1 then
		vpmtimer.AddTimer 500, "continuebonus '"
		exit Sub
	end if
	Set rBall = ActiveBall
	MyTroughTreasureAdd(rball)
	Me.TimerEnabled = 1
	LowerFlippersActive = False
	duuus()
	ChestDir = 1
	ChestupTimer.enabled = 1
end sub

sub continuebonus()
	BSTreasure.AddBall Kicker007
	BSTreasure.ExitSol_On
end sub

Sub BackToPlayfield()
	LowerFlippersActive = False
'	DMD "", "", "100k", eNone, eNone, eNone, 1000, True, ""
'	addscore 100000
	BarKickout 50, 160, 7, 0
	vpmtimer.addtimer 1100, "startB2S(3) '"
End Sub

Sub Kicker007_Timer
	Do While rBall.Z > 0
		rBall.Z = rBall.Z -5
		Exit Sub
	Loop
	Me.TimerEnabled = 0
	Me.Enabled = 1
	Me.DestroyBall
	FlashForMs Flasher002, 2100, 50, 0
	vpmtimer.addtimer 2100, "BackToPlayfield '" 
End Sub

sub Target004_hit()
	playsound "hitmining"
	schatkistshaker()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	treasurfindy = treasurfindy	 + 1
	countr6 = countr6 + 1
	Updatechest()
	checktreasurecomplete
end sub

Sub SchatkistShaker()
	SchatkistShake = 3
	SchatkistTimer.Enabled = True
End Sub

Sub SchatkistTimer_Timer()
	schatkistbelow.Transz = SchatkistShake / 2
	If SchatkistShake = 0 Then Me.Enabled = False:Exit Sub
	If SchatkistShake <0 Then
		SchatkistShake = ABS(SchatkistShake)- 0.1
	Else
		SchatkistShake = - SchatkistShake + 0.1
	End If
End Sub

sub checktreasurecomplete()
	If treasurfindy = 6 then
		RightFlipper001.RotatetoStart
		LeftFlipper001.RotatetoStart
		LowerFlippersActive = False
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		playsound "treasurecomplete"
		StopmodeEndofBall()
		li033.state = 1
		treasurfindy = 0
		AddMultiball 1
		Status = "Normal"
		countr6 = 0
		Updatechest()
		ChangeSong()
		treasuresfound = treasuresfound + 1
		Updatetreasurecountr()
		vpmtimer.addtimer 250, "startB2S(3) '"
		If BarUnlocked = 1 Then li007.state = 2
		If VoodooUnlocked = 1 Then li011.state = 2
	end if
end sub

Sub Updatechest()
	For Each Schat in CoSchatKist
		Schat.Visible = 0
	Next
	select case countr6
		case 0 : schatkist001.Visible=1
		case 1 : schatkist002.Visible=1
		case 2 : schatkist003.Visible=1
		case 3 : schatkist004.Visible=1
		case 4 : schatkist005.Visible=1
		case 5 : schatkist006.Visible=1
		case 6 : schatkist007.Visible=1:addimagechest()
	end Select
End Sub

sub addimagechest()
	imagechest = imagechest + 1
	updateimagechest()
End Sub

sub updateimagechest()
	select case imagechest
		case 0 :chestimage1
		case 1 :chestimage2
		case 2 :chestimage3
		case 3 :chestimage4
		case 4 :chestimage5
		case 5 :chestimage6
		case 6 :chestimage7
	end Select
End Sub

Dim Schat
sub chestimage1()
	For Each Schat in CoSchatKist
		Schat.Image = "schatkist5"
	Next
end sub

sub chestimage2
	For Each Schat in CoSchatKist
		Schat.Image = "schatkist6"
	Next
end sub

sub chestimage3
	For Each Schat in CoSchatKist
		Schat.Image = "schatkist7"
	Next
end sub

sub chestimage4
	For Each Schat in CoSchatKist
		Schat.Image = "schatkist8"
	Next
end sub

sub chestimage5
	For Each Schat in CoSchatKist
		Schat.Image = "schatkist9"
	Next
end sub

sub chestimage6
	For Each Schat in CoSchatKist
		Schat.Image = "schatkist10"
	Next
end sub

sub chestimage7
	For Each Schat in CoSchatKist
		Schat.Image = "schatkist5"
	Next
	imagechest = 0
end sub

'********bar fight mode***************

Dim mFist
sub BarFight()
	StopSong()
	PlaySong "barquest2" 
	mode1TimerCount = 90
	mode1timer.Enabled = 1
	itemrotztimer.Enabled = 1
	enablefists()
	BSBarToBar.ExitSol_On
	li034.state = 2
	Status = "Fight"
end sub

sub enablefists()
	Dim X
	'tim001.Enabled = 1
	'tim002.Enabled = 1
	'tim003.Enabled = 1
	'tim004.Enabled = 1
	'tim005.Enabled = 1
	'tim006.Enabled = 1 
	tfist001.Enabled = 1
	tfist002.Enabled = 1
	tfist003.Enabled = 1
	tfist004.Enabled = 1
	tfist005.Enabled = 1
	For Each X in Fists
		X.Visible = 1
	Next
end sub

Sub tfist001_Hit()
	tfist001.enabled = 0
	movefistdown(1)
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "fisthurt"
	mfist = mfist + 1
	addextratime()
	checkbonusfist()
end sub

sub movefistdown(which)
	Select Case which
		Case 1
			fist001.Visible = 0
		Case 2
			fist002.Visible = 0
		Case 3
			fist003.Visible = 0
		Case 4
			fist004.Visible = 0
		Case 5
			fist005.Visible = 0
	End Select
end sub

Sub tfist002_Hit()
	tfist002.enabled = 0
	movefistdown(2)
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "fisthurt"
	mfist = mfist + 1
	addextratime()
	checkbonusfist()
end sub

Sub tfist003_Hit()
	tfist003.enabled = 0
	movefistdown(3)
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "fisthurt"
	mfist = mfist + 1
	addextratime()
	checkbonusfist()
end sub

Sub tfist004_Hit()
	tfist004.enabled = 0
	movefistdown(4)
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "fisthurt"
	mfist = mfist + 1
	addextratime()
	checkbonusfist()
end sub

Sub tfist005_Hit()
	tfist005.enabled = 0
	movefistdown(5)
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "fisthurt"
	mfist = mfist + 1
	addextratime()
	checkbonusfist()
end sub

sub checkbonusfist()
	If mfist = 5 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		playsound "won"
		StopmodeEndofBall()
		li034.state = 1
		mfist = 0
		AddMultiball 1
		Status = "Normal"
		ChangeSong()
		If BarUnlocked = 1 Then li007.state = 2
		If VoodooUnlocked = 1 Then li011.state = 2
	end if
end sub

'********bar coinfrenzy mode***************

Dim mCoin
sub CoinFrenzy()
	StopSong()
	PlaySong "barquest2" 
	mode1TimerCount = 90
	mode1timer.Enabled = 1
	itemrotztimer.Enabled = 1
	enablecoins()
	BSBarToBar.ExitSol_On
	li007.state = 0
	li035.state = 2
	Status = "Coin"
end sub

sub enablecoins()
	Dim X
	tcoin001.Enabled = 1
	tcoin002.Enabled = 1
	tcoin003.Enabled = 1
	tcoin004.Enabled = 1
	tcoin005.Enabled = 1
	For Each X in Coins
		X.Visible = 1
	Next
end sub

Sub tcoin001_Hit()
	tcoin001.enabled = 0
	mcoin = mcoin + 1
	movecoindown(1)
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "coinhurt"
	addextratime()
	checkbonuscoin()
	coinsearned = coinsearned + 1
	Updatecoinscountr()
	vpmtimer.addtimer 250, "startB2S(3) '"
end sub

sub movecoindown(which)
	Select Case which
		Case 1
			coin001.Visible = 0
		Case 2
			coin002.Visible = 0
		Case 3
			coin003.Visible = 0
		Case 4
			coin004.Visible = 0
		Case 5
			coin005.Visible = 0
	End Select
end sub

Sub tcoin002_Hit()
	tcoin002.enabled = 0
	mcoin = mcoin + 1
	movecoindown(2)
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "coinhurt"
	addextratime()
	checkbonuscoin()
	coinsearned = coinsearned + 1
	Updatecoinscountr()
	vpmtimer.addtimer 250, "startB2S(3) '"
end sub

Sub tcoin003_Hit()
	tcoin003.enabled = 0
	mcoin = mcoin + 1
	movecoindown(3)
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "coinhurt"
	addextratime()
	checkbonuscoin()
	coinsearned = coinsearned + 1
	Updatecoinscountr()
	vpmtimer.addtimer 250, "startB2S(3) '"
end sub

Sub tcoin004_Hit()
	tcoin004.enabled = 0
	mcoin = mcoin + 1
	movecoindown(4)
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "coinhurt"
	addextratime()
	checkbonuscoin()
	coinsearned = coinsearned + 1
	Updatecoinscountr()
	vpmtimer.addtimer 250, "startB2S(3) '"
end sub

Sub tcoin005_Hit()
	tcoin005.enabled = 0
	movecoindown(5)
	mcoin = mcoin + 1
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "coinhurt"
	addextratime()
	checkbonuscoin()
	coinsearned = coinsearned + 1
	Updatecoinscountr()
	vpmtimer.addtimer 250, "startB2S(3) '"
end sub

sub checkbonuscoin()
	If mcoin = 5 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		playsound "won"
		StopmodeEndofBall()
		li035.state = 1
		mcoin = 0
		AddMultiball 1
		Status = "Normal"
		ChangeSong()
		If BarUnlocked = 1 Then li007.state = 2
		If VoodooUnlocked = 1 Then li011.state = 2
	end if
end sub

'********bar duels mode***************

sub Duels()
	li036.state = 2
	li036.state = 1
	BSBarToBar.ExitSol_On
	Status = "Duels"
	Status = "Normal"
	If BarUnlocked = 1 Then li007.state = 2
	If VoodooUnlocked = 1 Then li011.state = 2
end sub

'**************
' SlotMachine Voodoo Lady
'**************
'Dim TempBall
Dim SlotAward2, SlotValue2
SlotValue2 = 0

SlotAward2 = Array("v1", "v2", "v3", "v4", "v5")

Sub StartVoodooSlotmachine()
	startB2S(7)
	PlayQuoteVoodoo()
	Dim i
	DMDFlush()
	For i = 0 to 4
		DMD "", "", SlotAward2(i), eNone, eNone, eNone, 50, False, "fx_spinner"
	Next
	vpmtimer.AddTimer 2000, "GiveSlotAwardVoodoo '"
	playsound "dropopen"
	'Stop the lights blinking and reset the INSULT variables
	li047.state = 0
	li037.state = 0
	li038.state = 0
	li039.state = 0
	li040.state = 0
	li041.state = 0
	li042.state = 0
	li011.state = 0
	li007.state = 0
	StopSong()
	BSVoodooToVoodoo.AddBall Kicker005
End Sub

Dim VoodooSlot:VoodooSlot = 0
Sub GiveSlotAwardVoodoo()
	DMDFlush()
	Randomize()
	SlotValue2 = INT(RND * 5 + 1)	'randomly pick a mode
	DMD "", "", SlotAward2(SlotValue2-1), eNone, eNone, eNone, 500, True, ""
	Select Case SlotValue2
		Case 1
			SlotValue2 = 1	'VoodooMagic
		Case 2
			SlotValue2 = 2	'Chicken
		Case 3
			SlotValue2 = 4	'Mushroom
		Case 4
			SlotValue2 = 8	'Dolls
		Case 5
			SlotValue2 = 16	'Ears
	End Select
	'Check if this mode has already been played
	Do While (SlotValue2 AND VoodooSlot) > 0	'We've already done this mode So keep randomly picking one until we get a mode we haven't done yet
		SlotValue2 = INT(RND * 5 + 1)
		DMDFlush()
		DMD "", "", SlotAward2(SlotValue2-1), eNone, eNone, eNone, 500, True, ""
		Select Case SlotValue2
			Case 1
				SlotValue2 = 1	'VoodooMagic
			Case 2
				SlotValue2 = 2	'Chicken
			Case 3
				SlotValue2 = 4	'Mushroom
			Case 4
				SlotValue2 = 8	'Dolls
			Case 5
				SlotValue2 = 16	'Ears
		End Select
	Loop
	Select Case SlotValue2
		Case 1
			vpmTimer.AddTimer 1000, "VoodooMagic() '"		'VoodooMagic()
		Case 2
			vpmTimer.AddTimer 1000, "VoodooChicken() '"		'VoodooChicken()
		Case 4
			vpmTimer.AddTimer 1000, "VoodooMushroom() '"	'VoodooMushroom()
		Case 8
			vpmTimer.AddTimer 1000, "VoodooDolls() '"		'VoodooDolls()
		Case 16
			vpmTimer.AddTimer 1000, "VoodooEars() '"		'VoodooEars()
	End Select
End Sub

Sub VoodooPicker()
	Randomize()
	WheelSpeed = Int(2.5*Rnd+1)
	SpinningWheel.enabled = 1
End Sub

'********voodoo magic mode***************
Sub TrMagnet_Hit()
	RovingMagnet.AddBall ActiveBall
End Sub

Sub TrMagnet_UnHit()
	RovingMagnet.RemoveBall ActiveBall
	'If there's no balls in the magnet and the motor has stopped, then you successfully hit the ball
	If UBound(RovingMagnet.Balls) = -1 AND MotorTimer.Enabled = True Then
		MagnetsOffMain()
		li002.State = 1
		VoodooSlot = VoodooSlot OR 1
		If VoodooSlot = 31 Then
			VoodooUnlocked = 0
			li047.state = 0
		End If
	End If
End Sub

sub VoodooMagic()
	Dim X, Y
	VoodooKicker()
	'Make sure the magnet is in the correct spot
	RovingMagnet.X = Kicker002.X
	RovingMagnet.Y = Kicker002.Y
	PlaySong "voodooquests"
	LowerTarget013()
	li002.state = 2
	Status = "Magic"
end sub

Sub VoodooKicker()
	Dim X, Y
	Kicker002.CreateSizedball(BallSize / 2).ID = 666
	BallsOnPlayfield = BallsOnPlayfield + 1
	For Each X in MagicDust
		X.X = Kicker002.X
		X.Y = Kicker002.Y
		X.Visible = 1
	Next
	Status = "Magic"
	vpmtimer.addtimer 500, "MagnetsOnMain() '"
End Sub

Sub MagnetsOnMain()		' NudgeOkay=0 because nudge liberate Voodoo Ball...
	Dim TmpBall, X
	Playsound "fx_popper"
	MotorTimer.Enabled = 1
	tmpBall = GetBalls
	RovingMagnet.MagnetOn = 1
	Kicker002.kick 0, 15, 80
	Kicker002.Enabled = 0
	Playsound "fx_popper"
	vpmtimer.AddTimer 2000, "BSVoodooToVoodoo.ExitSol_On '"
	vpmtimer.addtimer 4000, "ResetTarget013 '"
	vpmtimer.addtimer 4000, "Kicker002Enable '"
End Sub

Sub MagnetsOffMain()
	Dim tmpballs, X
	RovingMagnet.MagnetOn = 0
	RovingMagnet.X = Kicker002.X
	RovingMagnet.Y = Kicker002.Y
	MagAngle = MagAngleOriginal
	MotorTimer.Enabled = 0
	For each X in RovingMagnet.Balls
		RovingMagnet.RemoveBall X
	Next
End Sub

Sub Kicker002Enable()
	Kicker002.Enabled = 1
End Sub

Dim MagAngle, MagAngleOriginal, MagCenterX, MagCenterY, MagRadius, MagSpeed
MagCenterX = 488
MagCenterY = 1664
MagRadius = 475
MagAngleOriginal = 315
MagAngle = MagAngleOriginal
MagSpeed = 0.0075
Sub MotorTimer_Timer()
	RovingMagnet.X = MagCenterX + dCos(MagAngle) * MagRadius
	RovingMagnet.Y = MagCenterY + dSin(MagAngle) * MagRadius
	MagAngle = MagAngle - MagSpeed
	If MagAngle <= 0 Then MagAngle = MagAngle + 360
	If RovingMagnet.Y > 1490 Then MagnetsOffMain()
End Sub

Sub GetRidOfVoodooBall(Ball)
	Dim X, XCoord, YCoord, ZCoord, TVelX, TVelY, TVelZ, TAngVelX, TAngVelY, TAngVelZ, TAngMomX, TAngMomY, TAngMomZ
	For Each X in MagicDust
		X.Visible = 0
		X.X = Kicker002.X
		X.Y = Kicker002.Y
	Next
	'If this is the voodoo ball, then just process the drain like normal
	If Ball.ID = 666 Then
	'Otherwise we need to swap the voodoo ball and ball that just drained
	Else
		Drain.Kick 0, 0, 0
		For Each X in GetBalls
			If X.ID = 666 Then
				'Get all info
				XCoord = X.X
				YCoord = X.Y
				ZCoord = X.Z
				TVelX = X.VelX
				TVelY = X.VelY
				TVelZ = X.VelZ
				TAngVelX = X.AngVelX
				TAngVelY = X.AngVelY
				TAngVelZ = X.AngVelZ
				TAngMomX = X.AngMomX
				TAngMomY = X.AngMomY
				TAngMomZ = X.AngMomz
				'Move it to right above the drain
				X.X = Drain.X
				X.Y = Drain.Y - 60
				X.Z = 0
				X.VelX = 0
				X.VelY = 0
				X.VelZ = 0
			End If
		Next
		'Move the ball that hit the drain to where voodoo ball was
		Ball.X = XCoord
		Ball.Y = YCoord
		Ball.Z = ZCoord
		Ball.VelX = TVelX
		Ball.VelY = TVelY
		Ball.VelZ = TvelZ
		Ball.AngVelX = TAngVelX
		Ball.AngVelY = TAngVelY
		Ball.AngVelZ = TAngVelZ
		Ball.AngMomX = TAngMomX
		Ball.AngMomY = TAngMomY
		Ball.AngMomZ = TAngMomZ
	End If
	Status = "Normal"
	If VoodooUnlocked = 1 Then li011.state = 2
	If BarUnlocked = 1 Then li007.state = 2
	StopModeEndOfBall()
	If VoodooSlot = 31 Then
		VoodooUnlocked = 0
		li047.state = 1
		li011.state = 0
	End If
End Sub

'********voodoo chicken mode***************

sub VoodooChicken()
	Status = "Chicken"
	StopSong()
	PlaySong "voodooquests" 
	mode1TimerCount = 90
	mode1timer.Enabled = 1
	enablechickens()
	BSVoodooToVoodoo.ExitSol_On
	TiChickenDance.Enabled = 1
	li028.state = 2
end sub

Dim WhichChicken, ChickenChecker
WhichChicken = 0
ChickenChecker = 0
sub enablechickens()
	If ChickenChecker = 31 Then
		CheckBonusChicken()
		Exit Sub
	End If
	Randomize()
	WhichChicken = INT(RND * 5) + 1
	Select Case WhichChicken
		Case 3
			WhichChicken = 4
		Case 4
			WhichChicken = 8
		Case 5
			WhichChicken = 16
	End Select
	Do While (WhichChicken AND ChickenChecker) > 0
		WhichChicken = INT(RND * 5) + 1
		Select Case WhichChicken
			Case 3
				WhichChicken = 4
			Case 4
				WhichChicken = 8
			Case 5
				WhichChicken = 16
		End Select
	Loop
	Select Case WhichChicken
		Case 1
			tchicken001.enabled = 1
			chicken001.Visible = 1
			chicken001.X = tchicken001.X
			chicken001.Y = tchicken001.Y
		Case 2
			tchicken002.enabled = 1
			chicken001.Visible = 1
			chicken001.X = tchicken002.X
			chicken001.Y = tchicken002.Y
		Case 4
			tchicken003.enabled = 1
			chicken001.Visible = 1
			chicken001.X = tchicken003.X
			chicken001.Y = tchicken003.Y
		Case 8
			tchicken004.enabled = 1
			chicken001.Visible = 1
			chicken001.X = tchicken004.X
			chicken001.Y = tchicken004.Y
		Case 16
			tchicken005.enabled = 1
			chicken001.Visible = 1
			chicken001.X = tchicken005.X
			chicken001.Y = tchicken005.Y
	End Select
	ChickenChecker = (ChickenChecker OR WhichChicken)
end sub

sub movechickendown()
	Dim X
	For Each X in Chickens
		X.Visible = 0
	Next
end sub

Sub tchicken001_Hit()
	tchicken001.enabled = 0
	movechickendown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "chickenhurt"
	addextratime()
	EnableChickens()
end sub

Sub tchicken002_Hit()
	tchicken002.enabled = 0
	movechickendown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "chickenhurt"
	addextratime()
	EnableChickens()
end sub

Sub tchicken003_Hit()
	tchicken003.enabled = 0
	movechickendown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "chickenhurt"
	addextratime()
	EnableChickens()
end sub

Sub tchicken004_Hit()
	tchicken004.enabled = 0
	movechickendown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "chickenhurt"
	addextratime()
	EnableChickens()
end sub

Sub tchicken005_Hit()
	tchicken005.enabled = 0
	movechickendown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "chickenhurt"
	addextratime()
	EnableChickens()
end sub

sub checkbonuschicken()
	If ChickenChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		playsound "won"
		StopmodeEndofBall()
		li028.state = 1
		AddMultiball 1
		Status = "Normal"
		ChangeSong()
		ChickenChecker = 0
		If VoodooUnlocked = 1 Then li011.state = 2
		If BarUnlocked = 1 Then li007.state = 2
		VoodooSlot = VoodooSlot OR 2
		TiChickenDance.Enabled = 0
		If VoodooSlot = 31 Then
			VoodooUnlocked = 0
			li047.state = 1
			li011.state = 0
		End If
	end if
end sub

Dim ChickenAngle, ChickenHeight, ChickenSpeed, ChickenLean
ChickenAngle = 0
ChickenHeight = 20
ChickenSpeed = 3
ChickenLean = 20
Sub TiChickenDance_Timer()
	Chicken001.Z = ABS(dSin(ChickenAngle) * ChickenHeight) + 10
	Chicken001.RotZ = dCos(ChickenAngle) * ChickenLean
	ChickenAngle = ChickenAngle + ChickenSpeed
	If ChickenAngle >=360 Then ChickenAngle = ChickenAngle - 360
End Sub

'********voodoo mushroom mode***************

sub VoodooMushroom()
	Status = "Mushroom"
	StopSong()
	PlaySong "voodooquests" 
	mode1TimerCount = 90
	mode1timer.Enabled = 1
	itemrotytimer.Enabled = 1
	enablemushrooms()
	BSVoodooToVoodoo.ExitSol_On
	li029.state = 2
end sub

Dim WhichMushroom, MushroomChecker
WhichMushroom = 0
MushroomChecker = 0
sub enablemushrooms()
	If MushroomChecker = 31 Then
		CheckBonusMushroom()
		Exit Sub
	End If
	Randomize()
	WhichMushroom = INT(RND * 5) + 1
	Select Case WhichMushroom
		Case 3
			WhichMushroom = 4
		Case 4
			WhichMushroom = 8
		Case 5
			WhichMushroom = 16
	End Select
	Do While (WhichMushroom AND MushroomChecker) > 0
		WhichMushroom = INT(RND * 5) + 1
		Select Case WhichMushroom
			Case 3
				WhichMushroom = 4
			Case 4
				WhichMushroom = 8
			Case 5
				WhichMushroom = 16
		End Select
	Loop
	Select Case WhichMushroom
		Case 1
			tmushroom001.enabled = 1
			Mushroom001.Visible = 1
			Mushroom001.X = tmushroom001.X
			Mushroom001.Y = tmushroom001.Y
			Mushroom001.Z = 30
		Case 2
			tmushroom002.enabled = 1
			Mushroom001.Visible = 1
			Mushroom001.X = tmushroom002.X
			Mushroom001.Y = tmushroom002.Y
			Mushroom001.Z = 130
		Case 4
			tmushroom003.enabled = 1
			Mushroom001.Visible = 1
			Mushroom001.X = tmushroom003.X
			Mushroom001.Y = tmushroom003.Y
			Mushroom001.Z = 30
		Case 8
			tmushroom004.enabled = 1
			Mushroom001.Visible = 1
			Mushroom001.X = tmushroom004.X
			Mushroom001.Y = tmushroom004.Y
			Mushroom001.Z = 30
		Case 16
			tmushroom005.enabled = 1
			Mushroom001.Visible = 1
			Mushroom001.X = tmushroom005.X
			Mushroom001.Y = tmushroom005.Y
			Mushroom001.Z = 30
	End Select
	MushroomChecker = (MushroomChecker OR WhichMushroom)
end sub

Sub tmushroom001_Hit()
	tmushroom001.enabled = 0
	movemushroomdown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "mushroomhurt"
	addextratime()
	EnableMushrooms()
end sub

sub MoveMushroomDown()
	Dim X
	For Each X in Mushrooms
		X.Visible = 0
	Next
end sub

Sub tmushroom002_Hit()
	tmushroom002.enabled = 0
	MoveMushroomDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "mushroomhurt"
	addextratime()
	EnableMushrooms()
end sub

Sub tmushroom003_Hit()
	tmushroom003.enabled = 0
	MoveMushroomDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "mushroomhurt"
	addextratime()
	EnableMushrooms()
end sub

Sub tmushroom004_Hit()
	tmushroom004.enabled = 0
	MoveMushroomDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "mushroomhurt"
	addextratime()
	EnableMushrooms()
end sub

Sub tmushroom005_Hit()
	tmushroom005.enabled = 0
	MoveMushroomDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "mushroomhurt"
	addextratime()
	EnableMushrooms()
end sub

sub checkbonusmushroom()
	If MushroomChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		playsound "won"
		StopmodeEndofBall()
		li029.state = 1
		AddMultiball 1
		Status = "Normal"
		ChangeSong()
		MushroomChecker = 0
		If VoodooUnlocked = 1 Then li011.state = 2
		If BarUnlocked = 1 Then li007.state = 2
		VoodooSlot = VoodooSlot OR 4
		If VoodooSlot = 31 Then
			VoodooUnlocked = 0
			li047.state = 1
			li011.state = 0
		End If
	end if
end sub

'********voodoo dolls mode***************

sub VoodooDolls()
	Status = "Dolls"
	StopSong
	PlaySong "voodooquests" 
	mode1TimerCount = 90
	mode1timer.Enabled = 1
	itemrotytimer.Enabled = 1
	enablevoodoodolls()
	BSVoodooToVoodoo.ExitSol_On
	li030.state = 2
end sub

Dim WhichDoll, DollChecker
WhichDoll = 0
DollChecker = 0
sub EnableVoodooDolls()
	If DollChecker = 31 Then
		CheckBonusVoodooDoll()
		Exit Sub
	End If
	Randomize()
	WhichDoll = INT(RND * 5) + 1
	Select Case WhichDoll
		Case 3
			WhichDoll = 4
		Case 4
			WhichDoll = 8
		Case 5
			WhichDoll = 16
	End Select
	Do While (WhichDoll AND DollChecker) > 0
		WhichDoll = INT(RND * 5) + 1
		Select Case WhichDoll
			Case 3
				WhichDoll = 4
			Case 4
				WhichDoll = 8
			Case 5
				WhichDoll = 16
		End Select
	Loop
	Select Case WhichDoll
		Case 1
			tvoodoodoll001.enabled = 1
			VoodooDoll001.Visible = 1
			VoodooDoll001.X = tvoodoodoll001.X
			VoodooDoll001.Y = tvoodoodoll001.Y
		Case 2
			tvoodoodoll002.enabled = 1
			VoodooDoll001.Visible = 1
			VoodooDoll001.X = tvoodoodoll002.X
			VoodooDoll001.Y = tvoodoodoll002.Y
		Case 4
			tvoodoodoll003.enabled = 1
			VoodooDoll001.Visible = 1
			VoodooDoll001.X = tvoodoodoll003.X
			VoodooDoll001.Y = tvoodoodoll003.Y
		Case 8
			tvoodoodoll004.enabled = 1
			VoodooDoll001.Visible = 1
			VoodooDoll001.X = tvoodoodoll004.X
			VoodooDoll001.Y = tvoodoodoll004.Y
		Case 16
			tvoodoodoll005.enabled = 1
			VoodooDoll001.Visible = 1
			VoodooDoll001.X = tvoodoodoll005.X
			VoodooDoll001.Y = tvoodoodoll005.Y
	End Select
	DollChecker = (DollChecker OR WhichDoll)
end sub

Sub tvoodoodoll001_Hit()
	tvoodoodoll001.enabled = 0
	MoveVoodooDollDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "voodoodollhurt"
	addextratime()
	EnableVoodooDolls()
end sub

sub MoveVoodooDollDown()
	VoodooDoll001.Visible = 0
end sub

Sub tvoodoodoll002_Hit()
	tvoodoodoll002.enabled = 0
	MoveVoodooDollDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "voodoodollhurt"
	addextratime()
	EnableVoodooDolls()
end sub

Sub tvoodoodoll003_Hit()
	tvoodoodoll003.enabled = 0
	MoveVoodooDollDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "voodoodollhurt"
	addextratime()
	EnableVoodooDolls()
end sub

Sub tvoodoodoll004_Hit()
	tvoodoodoll004.enabled = 0
	MoveVoodooDollDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "voodoodollhurt"
	addextratime()
	EnableVoodooDolls()
end sub

Sub tvoodoodoll005_Hit()
	tvoodoodoll005.enabled = 0
	MoveVoodooDollDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "voodoodollhurt"
	addextratime()
	EnableVoodooDolls()
end sub

sub CheckBonusVoodooDoll()
	If DollChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		playsound "won"
		StopmodeEndofBall()
		li030.state = 1
		AddMultiball 1
		Status = "Normal"
		ChangeSong()
		DollChecker = 0
		If VoodooUnlocked = 1 Then li011.state = 2
		If BarUnlocked = 1 Then li007.state = 2
		VoodooSlot = VoodooSlot OR 8
		If VoodooSlot = 31 Then
			VoodooUnlocked = 0
			li047.state = 1
			li011.state = 0
		End If
	end if
end sub

'********voodoo ears mode***************

sub VoodooEars()
	Status = "Ears"
	StopSong()
	PlaySong "voodooquests" 
	mode1TimerCount = 90
	mode1timer.Enabled = 1
	itemrotytimer.Enabled = 1
	EnableEars()
	BSVoodooToVoodoo.ExitSol_On
	li031.state = 2
end sub

Dim WhichEar, EarChecker
WhichEar = 0
EarChecker = 0
sub EnableEars()
	If EarChecker = 31 Then
		CheckBonusEar()
		Exit Sub
	End If
	Randomize()
	WhichEar = INT(RND * 5) + 1
	Select Case WhichEar
		Case 3
			WhichEar = 4
		Case 4
			WhichEar = 8
		Case 5
			WhichEar = 16
	End Select
	Do While (WhichEar AND EarChecker) > 0
		WhichEar = INT(RND * 5) + 1
		Select Case WhichEar
			Case 3
				WhichEar = 4
			Case 4
				WhichEar = 8
			Case 5
				WhichEar = 16
		End Select
	Loop
	Select Case WhichEar
		Case 1
			TEar001.enabled = 1
			Ear001.Visible = 1
			Ear001.X = TEar001.X
			Ear001.Y = TEar001.Y
		Case 2
			TEar002.enabled = 1
			Ear001.Visible = 1
			Ear001.X = TEar002.X
			Ear001.Y = TEar002.Y
		Case 4
			TEar003.enabled = 1
			Ear001.Visible = 1
			Ear001.X = TEar003.X
			Ear001.Y = TEar003.Y
		Case 8
			TEar004.enabled = 1
			Ear001.Visible = 1
			Ear001.X = TEar004.X
			Ear001.Y = TEar004.Y
		Case 16
			TEar005.enabled = 1
			Ear001.Visible = 1
			Ear001.X = TEar005.X
			Ear001.Y = TEar005.Y
	End Select
	EarChecker = (EarChecker OR WhichEar)
end sub

Sub tear001_Hit()
	tear001.enabled = 0
	MoveEarDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "earhurt"
	addextratime()
	EnableEars()
end sub

sub MoveEarDown()
	Dim X
	For Each X in Ears
		X.Visible = 0
	Next
end sub

Sub tear002_Hit()
	tear002.enabled = 0
	MoveEarDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "earhurt"
	addextratime()
	EnableEars()
end sub

Sub tear003_Hit()
	tear003.enabled = 0
	MoveEarDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "earhurt"
	addextratime()
	EnableEars()
end sub

Sub tear004_Hit()
	tear004.enabled = 0
	MoveEarDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "earhurt"
	addextratime()
	EnableEars()
end sub

Sub tear005_Hit()
	tear005.enabled = 0
	MoveEarDown()
	Score(CurrentPlayer) = Score(CurrentPlayer) + (10000*PFMultiplier)
	Playsound "earhurt"
	addextratime()
	EnableEars()
end sub

sub checkbonusear()
	If EarChecker = 31 then
		DMD "", "", "100k", eNone, eNone, eNone, 1000, True, ""
		Score(CurrentPlayer) = Score(CurrentPlayer) + (100000*PFMultiplier)
		playsound "won"
		StopmodeEndofBall()
		li031.state = 1
		AddMultiball 1
		Status = "Normal"
		ChangeSong()
		EarChecker = 0
		If VoodooUnlocked = 1 Then li011.state = 2
		If BarUnlocked = 1 Then li007.state = 2
		VoodooSlot = VoodooSlot OR 16
		If VoodooSlot = 31 Then
			VoodooUnlocked = 0
			li047.state = 1
			li011.state = 0
		End If
	end if
end sub

'********end of ball stop missions + add time ***************
sub stopquests()
	Dim X
	itemrotytimer.Enabled = 0
	itemrotztimer.Enabled = 0
	Status = "Normal"
	bonustime = 0
	tchicken001.enabled = 0
	tchicken002.enabled = 0
	tchicken003.enabled = 0
	tchicken004.enabled = 0
	tchicken005.enabled = 0
	tear001.enabled = 0
	tear002.enabled = 0
	tear003.enabled = 0
	tear004.enabled = 0
	tear005.enabled = 0
	tmushroom001.enabled = 0
	tmushroom002.enabled = 0
	tmushroom003.enabled = 0
	tmushroom004.enabled = 0
	tmushroom005.enabled = 0
	tvoodoodoll001.enabled = 0
	tvoodoodoll002.enabled = 0
	tvoodoodoll003.enabled = 0
	tvoodoodoll004.enabled = 0
	tvoodoodoll005.enabled = 0
	tcoin001.Enabled = 0
	tcoin002.Enabled = 0
	tcoin003.Enabled = 0
	tcoin004.Enabled = 0
	tcoin005.Enabled = 0
	tfist001.Enabled = 0
	tfist002.Enabled = 0
	tfist003.Enabled = 0
	tfist004.Enabled = 0
	tfist005.Enabled = 0
	For Each X in QuestLights
		If X.State = 2 Then X.State = 0
	Next
	For Each X in Fists
		X.Visible = 0
	Next
	For Each X in Coins
		X.Visible = 0
	Next
	For Each X in Chickens
		X.Visible = 0
	Next
	For Each X in Mushrooms
		X.Visible = 0
	Next
	For Each X in Ears
		X.Visible = 0
	Next
	For Each X in Dolls
		X.Visible = 0
	Next
end sub

sub addextratime()
	if mode1TimerCount > 90 then exit sub
	mode1TimerCount = mode1TimerCount + 5
end sub

Sub tim001_Hit()
	if mode1TimerCount > 90 then exit sub
	mode1TimerCount = mode1TimerCount + 2
end sub

Sub tim002_Hit()
	if mode1TimerCount > 90 then exit sub
	mode1TimerCount = mode1TimerCount + 2
end sub

Sub tim003_Hit()
	if mode1TimerCount > 90 then exit sub
	mode1TimerCount = mode1TimerCount + 2
end sub

Sub tim004_Hit()
	if mode1TimerCount > 90 then exit sub
	mode1TimerCount = mode1TimerCount + 2
end sub

Sub tim005_Hit()
	if mode1TimerCount > 90 then exit sub
	mode1TimerCount = mode1TimerCount + 2
end sub

Sub tim006_Hit()
	if mode1TimerCount > 90 then exit sub 
	mode1TimerCount = mode1TimerCount + 2
end sub

'********************************
'		 Digital clock
'********************************

Dim ClockDigits(4), ClockChars(10)

ClockDigits(0) = Array(a00, a02, a05, a06, a04, a01, a03) 'clock left digit
ClockDigits(1) = Array(a10, a12, a15, a16, a14, a11, a13)
ClockChars(0) = Array(1, 1, 1, 1, 1, 1, 0)				  '0
ClockChars(1) = Array(0, 1, 1, 0, 0, 0, 0)				  '1
ClockChars(2) = Array(1, 1, 0, 1, 1, 0, 1)				  '2
ClockChars(3) = Array(1, 1, 1, 1, 0, 0, 1)				  '3
ClockChars(4) = Array(0, 1, 1, 0, 0, 1, 1)				  '4
ClockChars(5) = Array(1, 0, 1, 1, 0, 1, 1)				  '5
ClockChars(6) = Array(1, 0, 1, 1, 1, 1, 1)				  '6
ClockChars(7) = Array(1, 1, 1, 0, 0, 0, 0)				  '7
ClockChars(8) = Array(1, 1, 1, 1, 1, 1, 1)				  '8
ClockChars(9) = Array(1, 1, 1, 1, 0, 1, 1)				  '9

Sub UpdateClock(myTime)
	Dim a, b, i
	a = myTime \ 10
	b = myTime MOD 10
	For i = 0 to 6
		ClockDigits(0)(i).State = ClockChars(a)(i)
		ClockDigits(1)(i).State = ClockChars(b)(i)
	Next
End Sub

Sub TurnOffClock
	Dim i
	For i = 0 to 6
		ClockDigits(0)(i).State = 0
		ClockDigits(1)(i).State = 0
	Next
End Sub

'clocktimer
Sub mode1timer_Timer
	mode1TimerCount = mode1TimerCount - 1
	UpdateClock mode1TimerCount
	If mode1TimerCount = 30 Then PlaySound "30s"
	If mode1TimerCount = 10 Then PlaySound "10s"
	If mode1TimerCount = 0 Then
	DMD "", "", "if", eNone, eNone, eNone, 1000, True, ""
		PlaySound "lost"
		Stopmode1()
	End If
End Sub

Sub mode2timer_Timer
	mode2TimerCount = mode2TimerCount - 1
	UpdateClock mode2TimerCount
	If mode2TimerCount = 0 Then
	DMD "", "", "tf", eNone, eNone, eNone, 1000, True, ""
		PlaySound "treasurefailed"
		Stopmode2
	End If
End Sub

Sub Stopmode1()
	mode1timer.Enabled = 0
	stopquests()
	StopSong()
	UpdateMusicNow()
	TurnOffClock()
End Sub

Sub Stopmode2()
	RightFlipper001.RotatetoStart
	LeftFlipper001.RotatetoStart
	LowerFlippersActive = False
	mode2timer.Enabled = 0
	stopquests()
	StopSong
	UpdateMusicNow()
	TurnOffClock()
End Sub

Sub StopmodeEndofBall()
	mode1timer.Enabled = 0
	mode2timer.Enabled = 0
	StopSong()
	itemrotytimer.Enabled = 0
	itemrotztimer.Enabled = 0
	'UpdateMusicNow
	TurnOffClock()
	stopquests()
	Status = "Normal"
End Sub

'************************************************
'**************schatkist Animation*****************
'************************************************
Dim ChestDir, ChestPos

sub ChestdownTimer_Timer
	ChestupTimer.enabled = 0
	If treasurechest1.RotX < 75 Then treasurechest1.RotX = TreasureChest1.RotX + 4	
	If treasurechest.z > -85 then 
		treasurechest.z = treasurechest.z -4
	Else
		ChestdownTimer.Enabled = 0
	End If
	If Not (eBall Is Nothing) Then
		If eBall.Z > 0 Then
			eBall.Z = eBall.Z - 10
			Exit Sub
		End If
		MyTroughTreasureAdd(eBall)
		Set eBall = Nothing
		Kicker006.DestroyBall
		TreasureKickout 1050, 8, 38, 0
	End If
end sub

sub ChestupTimer_Timer
	ChestdownTimer.enabled = 0
	If treasurechest1.RotX > 0 Then treasurechest1.RotX = TreasureChest1.RotX - 4
	If treasurechest1.RotX < 0 Then Treasurechest1.RotX = 0
	If treasurechest.z < -1 Then
		treasurechest.z = treasurechest.z + 4
	Else
		ChestupTimer.Enabled = 0
	End If
	If treasurechest.Z > -0.5 Then treasurechest.z = 0.5
end sub

sub duuus()
	playsound "LIFT2"
end sub

'************************************************
'***************turning objects******************
'************************************************
Sub itemrotztimer_Timer
   coin001.Rotz = coin001.Rotz + 1
   if coin001.Rotz > 360 then
	   coin001.Rotz = 1
   end if
   coin002.Rotz = coin002.Rotz + 1
   if coin002.Rotz > 360 then
	   coin002.Rotz = 1
   end if
   coin003.Rotz = coin003.Rotz + 1
   if coin003.Rotz > 360 then
	   coin003.Rotz = 1
   end if
   coin004.Rotz = coin004.Rotz + 1
   if coin004.Rotz > 360 then
	   coin004.Rotz = 1
   end if
   coin005.Rotz = coin005.Rotz + 1
   if coin005.Rotz > 360 then
	   coin005.Rotz = 1
   end if
   fist001.Rotz = fist001.Rotz + 1
   if fist001.Rotz > 360 then
	   fist001.Rotz = 1
   end if
   fist002.Rotz = fist002.Rotz + 1
   if fist002.Rotz > 360 then
	   fist002.Rotz = 1
   end if
   fist003.Rotz = fist003.Rotz + 1
   if fist003.Rotz > 360 then
	   fist003.Rotz = 1
   end if
   fist004.Rotz = fist004.Rotz + 1
   if fist004.Rotz > 360 then
	   fist004.Rotz = 1
   end if
   fist005.Rotz = fist005.Rotz + 1
   if fist005.Rotz > 360 then
	   fist005.Rotz = 1
   end if
   sword001.Rotz = sword001.Rotz + 1
   if sword001.Rotz > 360 then
	   sword001.Rotz = 1
   end if
   book.Rotz = book.Rotz + 1
   if book.Rotz > 360 then
	   book.Rotz = 1
   end if
end sub

Dim RotSpeed:RotSpeed = 1
Sub itemrotytimer_Timer()
	Dim X
	For Each X in Dolls
		X.RotY = X.RotY + RotSpeed
		If X.RotY > 360 Then X.RotY = X.RotY - 360
	Next
	For Each X in Ears
		X.RotY = X.RotY + RotSpeed
		If X.RotY > 360 Then X.RotY = X.RotY - 360
	Next
	For Each X in Mushrooms
		X.RotY = X.RotY + RotSpeed
		If X.RotY > 360 Then X.RotY = X.RotY - 360
	Next
	Map.RotY = Map.RotY + 1
	if Map.RotY > 360 then
		Map.RotY = 1
	end if
	compas.RotY = compas.RotY + 1
	if compas.RotY > 360 then
		compas.RotY = 1
	end if
	bone.RotY = bone.RotY + 1
	if bone.RotY > 360 then
		bone.RotY = 1
	end if
	mok.RotY = mok.RotY + 1
	if mok.RotY > 360 then
		mok.RotY = 1
	end if
	keysitem.RotY = keysitem.RotY + 1
	if keysitem.RotY > 360 then
		keysitem.RotY = 1
	end if
	skully.RotY = skully.RotY + 1
	if skully.RotY > 360 then
		skully.RotY = 1
	end if
end sub

'************************************************
'**************3d animations*****************
'************************************************

'Moved to the gametimer since the monkey animations are always moving

'************************************************
'**************b2s counters*****************
'************************************************

sub Updatemonkeycountr
select case monkeybattle
				case 0 : monkeynbr0:monkeynbr00
				case 1 : startB2S(51)
				case 2 : startB2S(52)
				case 3 : startB2S(53)
				case 4 : startB2S(54)
				case 5 : startB2S(55):punten2
				case 6 : startB2S(56)
				case 7 : startB2S(57)
				case 8 : startB2S(58)
				case 9 : startB2S(59)
				case 10 : monkeynbr0:monkeynbr10
				case 11 : startB2S(51)
				case 12 : startB2S(52)
				case 13 : startB2S(53)
				case 14 : startB2S(54)
				case 15 : startB2S(55):punten6
				case 16 : startB2S(56)
				case 17 : startB2S(57)
				case 18 : startB2S(58)
				case 19 : startB2S(59)
				case 20 : monkeynbr0:monkeynbr20
				case 21 : startB2S(51)
				case 22 : startB2S(52)
				case 23 : startB2S(53)
				case 24 : startB2S(54)
				case 25 : startB2S(55):punten10
				case 26 : startB2S(56)
				case 27 : startB2S(57)
				case 28 : startB2S(58)
				case 29 : startB2S(59)
				case 30 : monkeynbr0:monkeynbr30
				case 31 : startB2S(51)
				case 32 : startB2S(52)
				case 33 : startB2S(53)
				case 34 : startB2S(54)
				case 35 : startB2S(55)
				case 36 : startB2S(56)
				case 37 : startB2S(57)
				case 38 : startB2S(58)
				case 39 : startB2S(59)
				case 40 : monkeynbr0:monkeynbr40
				case 41 : startB2S(51)
				case 42 : startB2S(52)
				case 43 : startB2S(53)
				case 44 : startB2S(54)
				case 45 : startB2S(55)
				case 46 : startB2S(56)
				case 47 : startB2S(57)
				case 48 : startB2S(58)
				case 49 : startB2S(59)
				case 50 : monkeynbr0:monkeynbr50
				case 51 : startB2S(51)
				case 52 : startB2S(52)
				case 53 : startB2S(53)
				case 54 : startB2S(54)
				case 55 : startB2S(55)
				case 56 : startB2S(56)
				case 57 : startB2S(57)
				case 58 : startB2S(58)
				case 59 : startB2S(59)
				case 60 : monkeynbr0:monkeynbr60
				case 61 : startB2S(51)
				case 62 : startB2S(52)
				case 63 : startB2S(53)
				case 64 : startB2S(54)
				case 65 : startB2S(55)
				case 66 : startB2S(56)
				case 67 : startB2S(57)
				case 68 : startB2S(58)
				case 69 : startB2S(59)
				case 70 : monkeynbr0:monkeynbr70
				case 71 : startB2S(51)
				case 72 : startB2S(52)
				case 73 : startB2S(53)
				case 74 : startB2S(54)
				case 75 : startB2S(55)
				case 76 : startB2S(56)
				case 77 : startB2S(57)
				case 78 : startB2S(58)
				case 79 : startB2S(59)
				case 80 : monkeynbr0:monkeynbr80
				case 81 : startB2S(51)
				case 82 : startB2S(52)
				case 83 : startB2S(53)
				case 84 : startB2S(54)
				case 85 : startB2S(55)
				case 86 : startB2S(56)
				case 87 : startB2S(57)
				case 88 : startB2S(58)
				case 89 : startB2S(59)
				case 90 : monkeynbr0:monkeynbr90
				case 91 : startB2S(51)
				case 92 : startB2S(52)
				case 93 : startB2S(53)
				case 94 : startB2S(54)
				case 95 : startB2S(55)
				case 96 : startB2S(56)
				case 97 : startB2S(57)
				case 98 : startB2S(58)
				case 99 : startB2S(59)
			end Select
End Sub

sub punten2
	LightSeq001.StopPlay
	PFMultiplier = 2
	DMD "", "", "2x", eNone, eNone, eNone, 1000, True, "bonuss"
	li022.state = 1
end sub

sub punten6
	PFMultiplier = 6
	DMD "", "", "6x", eNone, eNone, eNone, 1000, True, "bonuss"
	li023.state = 0
	li024.state = 1
end sub

sub punten10
	PFMultiplier = 10
	DMD "", "", "10x", eNone, eNone, eNone, 1000, True, "bonuss"
	li025.state = 0
	li026.state = 1
end sub

sub monkeynbr0
	startB2S(50)
end sub

sub monkeynbr00
	vpmtimer.addtimer 500, "startB2S(40) '"
	vpmtimer.addtimer 750, "startB2S(11) '"
end sub

sub monkeynbr10
PFMultiplier = 4
	DMD "", "", "4x", eNone, eNone, eNone, 1000, True, "bonuss"
li022.state = 0
li023.state = 1
vpmtimer.addtimer 500, "startB2S(41) '"
vpmtimer.addtimer 750, "startB2S(3) '"
end sub

sub monkeynbr20
PFMultiplier = 8
	DMD "", "", "8x", eNone, eNone, eNone, 1000, True, "bonuss"
li024.state = 0
li025.state = 1
vpmtimer.addtimer 500, "startB2S(42) '"
vpmtimer.addtimer 750, "startB2S(3) '"
end sub

sub monkeynbr30
PFMultiplier = 12
	DMD "", "", "12x", eNone, eNone, eNone, 1000, True, "bonuss"
li026.state = 0
li027.state = 1
vpmtimer.addtimer 500, "startB2S(43) '"
vpmtimer.addtimer 750, "startB2S(3) '"
end sub

sub monkeynbr40
vpmtimer.addtimer 500, "startB2S(44) '"
vpmtimer.addtimer 750, "startB2S(3) '"
end sub

sub monkeynbr50
vpmtimer.addtimer 500, "startB2S(45) '"
vpmtimer.addtimer 750, "startB2S(3) '"
end sub

sub monkeynbr60
vpmtimer.addtimer 500, "startB2S(46) '"
vpmtimer.addtimer 750, "startB2S(3) '"
end sub

sub monkeynbr70
PFMultiplier = 14
vpmtimer.addtimer 500, "startB2S(47) '"
vpmtimer.addtimer 750, "startB2S(3) '"
end sub

sub monkeynbr80
PFMultiplier = 16
vpmtimer.addtimer 500, "startB2S(48) '"
vpmtimer.addtimer 750, "startB2S(3) '"
end sub

sub monkeynb90
PFMultiplier = 18
vpmtimer.addtimer 500, "startB2S(49) '"
vpmtimer.addtimer 750, "startB2S(3) '"
end sub

sub Updatetreasurecountr
select case treasuresfound
				case 0 : treasurenbr0:treasurenbr00
				case 1 : startB2S(31)
				case 2 : startB2S(32)
				case 3 : startB2S(33)
				case 4 : startB2S(34)
				case 5 : startB2S(35)
				case 6 : startB2S(36)
				case 7 : startB2S(37)
				case 8 : startB2S(38)
				case 9 : startB2S(39)
				case 10 : treasurenbr0:treasurenbr10
				case 11 : startB2S(31)
				case 12 : startB2S(32)
				case 13 : startB2S(33)
				case 14 : startB2S(34)
				case 15 : startB2S(35)
				case 16 : startB2S(36)
				case 17 : startB2S(37)
				case 18 : startB2S(38)
				case 19 : startB2S(39)
				case 20 : treasurenbr0:treasurenbr20
				case 21 : startB2S(31)
				case 22 : startB2S(32)
				case 23 : startB2S(33)
				case 24 : startB2S(34)
				case 25 : startB2S(35)
				case 26 : startB2S(36)
				case 27 : startB2S(37)
				case 28 : startB2S(38)
				case 29 : startB2S(39)
				case 30 : treasurenbr0:treasurenbr30
				case 31 : startB2S(31)
				case 32 : startB2S(32)
				case 33 : startB2S(33)
				case 34 : startB2S(34)
				case 35 : startB2S(35)
				case 36 : startB2S(36)
				case 37 : startB2S(37)
				case 38 : startB2S(38)
				case 39 : startB2S(39)
				case 40 : treasurenbr0:treasurenbr40
				case 41 : startB2S(31)
				case 42 : startB2S(32)
				case 43 : startB2S(33)
				case 44 : startB2S(34)
				case 45 : startB2S(35)
				case 46 : startB2S(36)
				case 47 : startB2S(37)
				case 48 : startB2S(38)
				case 49 : startB2S(39)
				case 50 : treasurenbr0:treasurenbr50
				case 51 : startB2S(31)
				case 52 : startB2S(32)
				case 53 : startB2S(33)
				case 54 : startB2S(34)
				case 55 : startB2S(35)
				case 56 : startB2S(36)
				case 57 : startB2S(37)
				case 58 : startB2S(38)
				case 59 : startB2S(39)
				case 60 : treasurenbr0:treasurenbr60
				case 61 : startB2S(31)
				case 62 : startB2S(32)
				case 63 : startB2S(33)
				case 64 : startB2S(34)
				case 65 : startB2S(35)
				case 66 : startB2S(36)
				case 67 : startB2S(37)
				case 68 : startB2S(38)
				case 69 : startB2S(39)
				case 70 : treasurenbr0:treasurenbr70
				case 71 : startB2S(31)
				case 72 : startB2S(32)
				case 73 : startB2S(33)
				case 74 : startB2S(34)
				case 75 : startB2S(35)
				case 76 : startB2S(36)
				case 77 : startB2S(37)
				case 78 : startB2S(38)
				case 79 : startB2S(39)
				case 80 : treasurenbr0:treasurenbr80
				case 81 : startB2S(31)
				case 82 : startB2S(32)
				case 83 : startB2S(33)
				case 84 : startB2S(34)
				case 85 : startB2S(35)
				case 86 : startB2S(36)
				case 87 : startB2S(37)
				case 88 : startB2S(38)
				case 89 : startB2S(39)
				case 90 : treasurenbr0:treasurenbr90
				case 91 : startB2S(31)
				case 92 : startB2S(32)
				case 93 : startB2S(33)
				case 94 : startB2S(34)
				case 95 : startB2S(35)
				case 96 : startB2S(36)
				case 97 : startB2S(37)
				case 98 : startB2S(38)
				case 99 : startB2S(39)
			end Select
End Sub

sub treasurenbr0
	startB2S(30)
End Sub

sub treasurenbr00
vpmtimer.addtimer 500, "startB2S(20) '"
vpmtimer.addtimer 750, "startB2S(11) '"
End Sub

sub treasurenbr10
vpmtimer.addtimer 500, "startB2S(21) '"
vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub treasurenbr20
vpmtimer.addtimer 500, "startB2S(22) '"
vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub treasurenbr30
vpmtimer.addtimer 500, "startB2S(23) '"
vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub treasurenbr40
vpmtimer.addtimer 500, "startB2S(24) '"
vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub treasurenbr50
vpmtimer.addtimer 500, "startB2S(25) '"
vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub treasurenbr60
vpmtimer.addtimer 500, "startB2S(26) '"
vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub treasurenbr70
vpmtimer.addtimer 500, "startB2S(27) '"
vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub treasurenbr80
vpmtimer.addtimer 500, "startB2S(28) '"
vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub treasurenbr90
vpmtimer.addtimer 500, "startB2S(29) '"
vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub Updatecoinscountr
select case coinsearned
				case 0 : coinsnbr0:coinsnbr00
				case 1 : startB2S(51)
				case 2 : startB2S(52)
				case 3 : startB2S(53)
				case 4 : startB2S(54)
				case 5 : startB2S(55)
				case 6 : startB2S(56)
				case 7 : startB2S(57)
				case 8 : startB2S(58)
				case 9 : startB2S(59)
				case 10 : coinsnbr0:coinsnbr00
				case 11 : startB2S(51)
				case 12 : startB2S(52)
				case 13 : startB2S(53)
				case 14 : startB2S(54)
				case 15 : startB2S(55)
				case 16 : startB2S(56)
				case 17 : startB2S(57)
				case 18 : startB2S(58)
				case 19 : startB2S(59)
				case 20 : coinsnbr0:coinsnbr00
				case 21 : startB2S(51)
				case 22 : startB2S(52)
				case 23 : startB2S(53)
				case 24 : startB2S(54)
				case 25 : startB2S(55)
				case 26 : startB2S(56)
				case 27 : startB2S(57)
				case 28 : startB2S(58)
				case 29 : startB2S(59)
				case 30 : coinsnbr0:coinsnbr0
				case 31 : startB2S(51)
				case 32 : startB2S(52)
				case 33 : startB2S(53)
				case 34 : startB2S(54)
				case 35 : startB2S(55)
				case 36 : startB2S(56)
				case 37 : startB2S(57)
				case 38 : startB2S(58)
				case 39 : startB2S(59)
				case 40 : coinsnbr0:coinsnbr0
				case 41 : startB2S(51)
				case 42 : startB2S(52)
				case 43 : startB2S(53)
				case 44 : startB2S(54)
				case 45 : startB2S(55)
				case 46 : startB2S(56)
				case 47 : startB2S(57)
				case 48 : startB2S(58)
				case 49 : startB2S(59)
				case 70 : coinsnbr0:coinsnbr0
				case 51 : startB2S(51)
				case 52 : startB2S(52)
				case 53 : startB2S(53)
				case 54 : startB2S(54)
				case 55 : startB2S(55)
				case 56 : startB2S(56)
				case 57 : startB2S(57)
				case 58 : startB2S(58)
				case 59 : startB2S(59)
				case 60 : coinsnbr0:coinsnbr0
				case 61 : startB2S(51)
				case 62 : startB2S(52)
				case 63 : startB2S(53)
				case 64 : startB2S(54)
				case 65 : startB2S(55)
				case 66 : startB2S(56)
				case 67 : startB2S(57)
				case 68 : startB2S(58)
				case 69 : startB2S(59)
				case 70 : coinsnbr0:coinsnbr0
				case 71 : startB2S(51)
				case 72 : startB2S(52)
				case 73 : startB2S(53)
				case 74 : startB2S(54)
				case 75 : startB2S(55)
				case 76 : startB2S(56)
				case 77 : startB2S(57)
				case 78 : startB2S(58)
				case 79 : startB2S(59)
				case 80 : coinsnbr0:coinsnbr0
				case 81 : startB2S(51)
				case 82 : startB2S(52)
				case 83 : startB2S(53)
				case 84 : startB2S(54)
				case 85 : startB2S(55)
				case 86 : startB2S(56)
				case 87 : startB2S(57)
				case 88 : startB2S(58)
				case 89 : startB2S(59)
				case 90 : coinsnbr0:coinsnbr0
				case 91 : startB2S(51)
				case 92 : startB2S(52)
				case 93 : startB2S(53)
				case 94 : startB2S(54)
				case 95 : startB2S(55)
				case 96 : startB2S(56)
				case 97 : startB2S(57)
				case 98 : startB2S(58)
				case 99 : startB2S(59)
			end Select
End Sub

sub coinsnbr0
	startB2S(70)
End Sub

sub coinsnbr00
vpmtimer.addtimer 500, "startB2S(60) '"
vpmtimer.addtimer 750, "startB2S(11) '"
End Sub

sub coinsnbr10
vpmtimer.addtimer 500, "startB2S(61) '"
vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub coinsnbr20
vpmtimer.addtimer 500, "startB2S(62) '"
vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub coinsnbr30
vpmtimer.addtimer 500, "startB2S(63) '"
vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub coinsnbr40
vpmtimer.addtimer 500, "startB2S(64) '"
vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub coinsnbr50
vpmtimer.addtimer 500, "startB2S(65) '"
vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub coinsnbr60
vpmtimer.addtimer 500, "startB2S(66) '"
vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub coinsnbr70
vpmtimer.addtimer 500, "startB2S(67) '"
vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub coinsnbr80
vpmtimer.addtimer 500, "startB2S(68) '"
vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub coinsnbr90
vpmtimer.addtimer 500, "startB2S(69) '"
vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub Updatebananascountr()
	select case bananasearned
		case 0 : bananasnbr0:bananasbr00
		case 1 : startB2S(91)
		case 2 : startB2S(92)
		case 3 : startB2S(93)
		case 4 : startB2S(94)
		case 5 : startB2S(95)
		case 6 : startB2S(96)
		case 7 : startB2S(97)
		case 8 : startB2S(98)
		case 9 : startB2S(99)
		case 10 : bananasnbr0:bananasbr10
		case 11 : startB2S(91)
		case 12 : startB2S(92)
		case 13 : startB2S(93)
		case 14 : startB2S(94)
		case 15 : startB2S(95)
		case 16 : startB2S(96)
		case 17 : startB2S(97)
		case 18 : startB2S(98)
		case 19 : startB2S(99)
		case 20 : bananasnbr0:bananasbr20
		case 21 : startB2S(91)
		case 22 : startB2S(92)
		case 23 : startB2S(93)
		case 24 : startB2S(94)
		case 25 : startB2S(95)
		case 26 : startB2S(96)
		case 27 : startB2S(97)
		case 28 : startB2S(98)
		case 29 : startB2S(99)
		case 30 : bananasnbr0:bananasbr30
		case 31 : startB2S(91)
		case 32 : startB2S(92)
		case 33 : startB2S(93)
		case 34 : startB2S(94)
		case 35 : startB2S(95)
		case 36 : startB2S(96)
		case 37 : startB2S(97)
		case 38 : startB2S(98)
		case 39 : startB2S(99)
		case 40 : bananasnbr0:bananasbr40
		case 41 : startB2S(91)
		case 42 : startB2S(92)
		case 43 : startB2S(93)
		case 44 : startB2S(94)
		case 45 : startB2S(95)
		case 46 : startB2S(96)
		case 47 : startB2S(97)
		case 48 : startB2S(98)
		case 49 : startB2S(99)
		case 50 : bananasnbr0:bananasbr50
		case 51 : startB2S(91)
		case 52 : startB2S(92)
		case 53 : startB2S(93)
		case 54 : startB2S(94)
		case 55 : startB2S(95)
		case 56 : startB2S(96)
		case 57 : startB2S(97)
		case 58 : startB2S(98)
		case 59 : startB2S(99)
		case 60 : bananasnbr0:bananasbr60
		case 61 : startB2S(91)
		case 62 : startB2S(92)
		case 63 : startB2S(93)
		case 64 : startB2S(94)
		case 65 : startB2S(95)
		case 66 : startB2S(96)
		case 67 : startB2S(97)
		case 68 : startB2S(98)
		case 69 : startB2S(99)
		case 70 : bananasnbr0:bananasbr70
		case 71 : startB2S(91)
		case 72 : startB2S(92)
		case 73 : startB2S(93)
		case 74 : startB2S(94)
		case 75 : startB2S(95)
		case 76 : startB2S(96)
		case 77 : startB2S(97)
		case 78 : startB2S(98)
		case 79 : startB2S(99)
		case 80 : bananasnbr0:bananasbr80
		case 81 : startB2S(91)
		case 82 : startB2S(92)
		case 83 : startB2S(93)
		case 84 : startB2S(94)
		case 85 : startB2S(95)
		case 86 : startB2S(96)
		case 87 : startB2S(97)
		case 88 : startB2S(98)
		case 89 : startB2S(99)
		case 90 : bananasnbr0:bananasbr90
		case 91 : startB2S(91)
		case 92 : startB2S(92)
		case 93 : startB2S(93)
		case 94 : startB2S(94)
		case 95 : startB2S(95)
		case 96 : startB2S(96)
		case 97 : startB2S(97)
		case 98 : startB2S(98)
		case 99 : startB2S(99)
	end Select
End Sub

sub bananasnbr0()
	startB2S(90)
End Sub

sub bananasbr00()
	vpmtimer.addtimer 500, "startB2S(80) '"
	vpmtimer.addtimer 750, "startB2S(11) '"
End Sub

sub bananasbr10()
	vpmtimer.addtimer 500, "startB2S(81) '"
	vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub bananasbr20()
	vpmtimer.addtimer 500, "startB2S(82) '"
	vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub bananasbr30()
	vpmtimer.addtimer 500, "startB2S(83) '"
	vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub bananasbr40()
	vpmtimer.addtimer 500, "startB2S(84) '"
	vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub bananasbr50()
	vpmtimer.addtimer 500, "startB2S(85) '"
	vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub bananasbr60()
	vpmtimer.addtimer 500, "startB2S(86) '"
	vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub bananasbr70()
	vpmtimer.addtimer 500, "startB2S(87) '"
	vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub bananasbr80()
	vpmtimer.addtimer 500, "startB2S(88) '"
	vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub bananasbr90()
	vpmtimer.addtimer 500, "startB2S(89) '"
	vpmtimer.addtimer 750, "startB2S(3) '"
End Sub

sub resetycoins()
	coinsearned = 0
	Updatecoinscountr
	vpmtimer.addtimer 1000, "resettreasures '"
end sub

sub resettreasures()
	treasuresfound = 0
	Updatetreasurecountr
	vpmtimer.addtimer 1000, "resetbananasy '"
end sub

Sub resetbananasy()
	bananasearned = 0
	Updatebananascountr
	vpmtimer.addtimer 1000, "resetmonkeybattlys '"
end sub

Sub resetmonkeybattlys()
	monkeybattle = 0
	Updatemonkeycountr
end sub
