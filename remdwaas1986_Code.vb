' ****************************************************************
'                       VISUAL PINBALL X
'                		PINBALL_TABLE_NAME
'                       Version 1.0.0
'						started 
' ****************************************************************

Option Explicit
Randomize

Const BallSize = 50    ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
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
		Controller.B2SSetData aB2S,1
	End If
End Sub

' Define any Constants
Const cGameName = "PINBALL_TABLE_NAME"
Const TableName = "PINBALL_TABLE_NAME"
Const myVersion = "1.0.0"
Const MaxPlayers = 4     ' from 1 to 4
Const BallSaverTime = 20 ' in seconds
Const MaxMultiplier = 3  ' limit to 3x in this game, both bonus multiplier and playfield multiplier
Const BallsPerGame = 5   ' usually 3 or 5
Const MaxMultiballs = 4  ' max number of balls during multiballs

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

' Define Game Control Variables
Dim BallsOnPlayfield
Dim BallsInLock
Dim BallsInHole

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


' core.vbs variables

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
	LoadEM
	Dim i
	'Randomize
	'Impulse Plunger as autoplunger
	Const IMPowerSetting = 36 ' Plunger Power
	Const IMTime = 1.1        ' Time in seconds for Full Plunge
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
	BallsInLock = 0
	BallsInHole = 0
	LastSwitchHit = ""
	Tilt = 0
	TiltSensitivity = 6
	Tilted = False
	bJustStarted = True
	' set any lights for the attract mode
	GiOff
	StartAttractMode
	'EndOfGame()
End Sub

'****************************************
' Real Time updatess using the GameTimer
'****************************************
'used for all the real time updates

Sub GameTimer_Timer
	RollingUpdate
	' add any other real time update subs, like gates or diverters
	FlipperLSh.Rotz = LeftFlipper.CurrentAngle
	FlipperRSh.Rotz = RightFlipper.CurrentAngle
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
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
		shootkanon
		Plunger.Pullback
		PlaySoundAt "fx_plungerpull", plunger
		PlaySoundAt "fx_reload", plunger
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

		If keycode = LeftFlipperKey Then SolLFlipper 1
		If keycode = RightFlipperKey Then SolRFlipper 1

		If keycode = StartGameKey Then
			If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then

				If(bFreePlay = True) Then
					PlayersPlayingGame = PlayersPlayingGame + 1
					TotalGamesPlayed = TotalGamesPlayed + 1
					DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "so_fanfare1"
				Else
					If(Credits> 0) then
						PlayersPlayingGame = PlayersPlayingGame + 1
						TotalGamesPlayed = TotalGamesPlayed + 1
						Credits = Credits - 1
						DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "so_fanfare1"
						If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
						Else
							' Not Enough Credits to start a game.
							DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "so_nocredits"
					End If
				End If
			End If
		End If
		Else ' If (GameInPlay)

			If keycode = StartGameKey Then
				If(bFreePlay = True) Then
					If(BallsOnPlayfield = 0) Then
						ResetForNewGame()
						UpdateMusicNow
					End If
				Else
					If(Credits> 0) Then
						If(BallsOnPlayfield = 0) Then
							Credits = Credits - 1
							If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
							ResetForNewGame()
							UpdateMusicNow
						End If
					Else
						' Not Enough Credits to start a game.
						DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "so_nocredits"
					End If
				End If
			End If
	End If ' If (GameInPlay)

'table keys
'If keycode = RightMagnaSave or keycode = LeftMagnasave Then ShowPost 
End Sub

Sub Table1_KeyUp(ByVal keycode)

	If keycode = PlungerKey Then
		kanonnormal
		smokeytimer.enabled = 1
		Plunger.Fire
		PlaySoundAt "fx_plunger", plunger
		'If bBallInPlungerLane Then PlaySoundAt "fx_fire", plunger
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
End Sub

sub shootkanon
kanon.TransX = 40
'vpmTimer.AddTimer 250, "kanonnormal '" 
end sub

sub kanonnormal
kanon.TransX = 0
end sub

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
'     Flippers
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

'Sub RotateLaneLightsLeft2
'    Dim TempState
'    TempState = li016.State
'    li016.State = li017.State
'    li017.State = li018.State
'    li018.State = li019.State
'	li019.State = li020.state
'    li020.state = TempState
'End Sub

'Sub RotateLaneLightsRight2
'    Dim TempState
'    TempState = li020.State
'    li020.State = li019.State
'    li019.State = li018.State
'    li018.State = li017.State
'	li017.State = li016.state
'    li016.state = TempState
'End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                    'Called when table is nudged
	Tilt = Tilt + TiltSensitivity                'Add to tilt count
	TiltDecreaseTimer.Enabled = True
	If(Tilt> TiltSensitivity) AND(Tilt <15) Then 'show a warning
		DMD "_", CL(1, "CAREFUL!"), "", eNone, eBlinkFast, eNone, 500, True, ""
	End if
	If Tilt> 15 Then 'If more that 15 then TILT the table
		Tilted = True
		'display Tilt
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
'       Bumper1.Force = 0
'       Bumper2.Force = 0
'		Bumper3.Force = 0
		LeftSlingshot.Disabled = 1
		RightSlingshot.Disabled = 1
	Else
		'turn back on GI and the lights
		GiOn
		LightSeqTilt.StopPlay
'        Bumper1.Force = 8
'        Bumper2.Force = 8
'		Bumper3.Force = 8
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
		If Song <> name Then
			StopSound Song
			Song = name
			PlaySound Song, -1, SongVolume
		End If
	End If
End Sub

Sub StopSong
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

Sub UpdateMusicNow
	Select Case UpdateMusic
		Case 0:PlaySong "1"
		Case 1:PlaySong "2"
		Case 2:PlaySong "3"
		Case 3:PlaySong "4"
		Case 4:PlaySong "5"
		Case 5:PlaySong "M_end"
		'Case 6:PlaySong "chooseplayer2"
	End Select
end sub

'********************
' Play random quotes
'********************

Sub PlayQuote
	Dim tmp
	tmp = INT(RND * 123) + 1
	PlaySound "HIT_" &tmp
End Sub

'**********************
'     GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'**********************

Dim OldGiState
OldGiState = -1   'start witht the Gi off

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
			GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
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
'                      Supporting Ball & Sound Functions
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
'   JP's VP10 Rolling Sounds + Ballshadow
' uses a collection of shadows, aBallShadow
'********************************************

Const tnob = 20 ' total number of balls
Const lob = 0   'number of locked balls
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

Sub RHelp1_Hit()
	StopSound "fx_metalrolling"
	PlaySoundAtBall "fx_ballrampdrop"
End Sub

Sub RHelp2_Hit()
	StopSound "fx_metalrolling"
	PlaySoundAtBall"fx_ballrampdrop"
End Sub


' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
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

	'reset variables
	bumperHits = 100

	UpdateMusic = 0
	'UpdateMusic = UpdateMusic + 6
	UpdateMusicNow

	' initialise Game variables
	Game_Init()
	
	' you may wish to start some music, play a sound, do whatever at this point
StopSong
PlaySound ""


	vpmtimer.addtimer 1500, "FirstBall '"
End Sub

' This is used to delay the start of a game to allow any attract sequence to

' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
	' reset the table for a new ball
	    startB2S(2)
	ResetForNewPlayerBall()
	' create a new ball in the shooters lane
	CreateNewBall()
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
	' make sure the correct display is upto date
	AddScore 0

	' set the current players bonus multiplier back down to 1X
	BonusMultiplier(CurrentPlayer) = 1
	'UpdateBonusXLights
	
' reset any drop targets, lights, game Mode etc..
	
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
	Countr = 1
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

	If BallsOnPlayfield> 1 Then
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

	' only process any of this if the table is not tilted.  (the tilt recovery
	' mechanism will handle any extra balls or end of game)

	'LightSeqAttract.Play SeqBlinking, , 5, 150

StopSong
'bonuscheckie

	Dim AwardPoints, TotalBonus, ii
	AwardPoints = 0
	TotalBonus = 0

	'If NOT Tilted Then
	If(Tilted = False) Then
		
		'Number of Target hits
'       AwardPoints = TargetBonus * 2000
'       TotalBonus = TotalBonus + AwardPoints
'       DMD CL(0, FormatScore(AwardPoints)), CL(1, "TARGET BONUS " & TargetBonus), "", eBlink, eNone, eNone, 300, False, "whip" <- with dmd scores otherwise only total bonus

		AwardPoints = TargetBonus * 2000
		TotalBonus = TotalBonus + AwardPoints

		AwardPoints = RampBonus1 * 25000
		TotalBonus = TotalBonus + AwardPoints

		AwardPoints = RampBonus2 * 25000
		TotalBonus = TotalBonus + AwardPoints

		AwardPoints = RampBonus3 * 25000
		TotalBonus = TotalBonus + AwardPoints

		AwardPoints = ALLRampBonus * 75000
		TotalBonus = TotalBonus + AwardPoints
  
		AwardPoints = MulitballBonus * 150000
		TotalBonus = TotalBonus + AwardPoints
 
		AwardPoints = BumperBonus * 100000
		TotalBonus = TotalBonus + AwardPoints
		
		DMD CL(0, FormatScore(TotalBonus) ), CL(1, "TOTAL BONUS" & BonusMultiplier(CurrentPlayer) ), "", eBlinkFast, eNone, eNone, 1000, True, "po_bonus7"
		TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
		
		AddScore TotalBonus

		' add a bit of a delay to allow for the bonus points to be shown & added up
		vpmtimer.addtimer 3200, "EndOfBall2 '"
	Else 'Si hay falta simplemente espera un momento y va directo a la segunta parte después de perder la bola
		BonusDelayTime = 100
		EndOfBall2
	End If
	'vpmtimer.addtimer BonusDelayTime, "EndOfBall2 '"
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
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
		DMD CL(0, "EXTRA BALL"), CL(1, "SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, "vo_extraball"

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
		'DMD CL(0, "GAME OVER") "", eNone, 13000, True, ""
DMD "", CL(1, "GAME OVER"), "", eNone, eNone, eNone, 13000, False, ""
		PlaySound ""
		' set the machine into game over mode
		vpmtimer.addtimer 13000, "EndOfGame() '"

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
'                      Drain / Plunger Functions
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' if only one then decrement the remaining count AND test for End of game
' if more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
'
Sub Drain_Hit()
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
	bAutoPlunger = True
			' yep, create a new ball in the shooters lane
			' we use the Addmultiball in case the multiballs are being ejected
	DMD CL(0, "BALL SAVED"), CL(1, "SHOOT AGAIN"), "", eBlink, eBlink, eNone, 800, True, ""
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
				ChangeSong
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
				StopEndOfBallMode
				vpmtimer.addtimer 200, "EndOfBall '" 'the delay is depending of the animation of the end of ball, since there is no animation then move to the end of ball
			End If
		End If
	End If
End Sub



' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub Trigger1_Hit()
If bAutoPlunger Then
		'debug.print "autofire the ball"
		PlungerIM.AutoFire
		DOF 121, DOFPulse
		PlaySoundAt "fx_fire", Trigger1
		bAutoPlunger = False
	End If	
'StopSong
	DMDScoreNow
	bBallInPlungerLane = True
	DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eBlink, eNone, 1000, True, ""
	If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
		EnableBallSaver BallSaverTime
		Else
		' show the message to shoot the ball in case the player has fallen sleep
		Trigger1.TimerEnabled = 1
	End If
End Sub

' The ball is released from the plunger

Sub Trigger1_UnHit()
	bBallInPlungerLane = False
	'LightEffect 4
	'ChangeSong
End Sub


Sub Trigger1_Timer
	DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eNone, eNone, 800, True, ""
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

' The ball saver timer has expired.  Turn it off AND reset the game flag
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
'                      Supporting Score Functions
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
	DMD "_", CL(1, ("EXTRA BALL WON") ), "", eNone, eBlink, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
	DOF 121, DOFPulse
	ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
	LightShootAgain.State = 1
	LightEffect 2
End Sub

'*****************************
'    Load / Save / Highscore
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
	'    Credits = Credits + 1
	'    DOF 125, DOFOn
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

	TempBotStr = "    > "
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

	TempBotStr = TempBotStr & " <    "
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
'   JP's Reduced Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and blinkfast
' 3 Lines, treats all 3 lines as text. 3rd line is just 1 character
' Example format:
' DMD "text1","text2","backpicture", eNone, eNone, eNone, 250, True, "sound"
' Short names:
' dq = display queue
' de = display effect
' *************************************************************************

Const eNone = 0        ' Instantly displayed
Const eScrollLeft = 1  ' scroll on from the right
Const eScrollRight = 2 ' scroll on from the left
Const eBlink = 3       ' Blink (blinks for 'TimeOn')
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
			For i = 0 to 35
				DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.dempty&dmd=2")
				Digits(i).Visible = False
			Next
			'digitgrid.Visible = False
			For i = 0 to 19 ' Top
				DMDScene.GetImage("Dig" & i).SetBounds 4 + i * 6, 3 + 16 + 2, 8, 8
			Next
			For i = 20 to 35 ' Bottom
				DMDScene.GetImage("Dig" & i).SetBounds (i - 20) * 8, 3, 8, 16
			Next
			FlexDMD.LockRenderThread
			FlexDMD.Stage.AddActor DMDScene
			FlexDMD.UnlockRenderThread
		End If
	End If


'Sub DMD_Init() 'default/startup values
	Dim i, j
	DMDFlush()
	deSpeed = 20
	deBlinkSlowRate = 5
	deBlinkFastRate = 2
	dCharsPerLine(0) = 16 'characters lower line
	dCharsPerLine(1) = 20 'characters top line
	dCharsPerLine(2) = 1  'characters back line
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
		tmp = RL(0, FormatScore(Score(Currentplayer) ) )
		tmp1 = CL(1, "PLAYER " & CurrentPlayer & "  BALL " & Balls)
		tmp2 = "bkborder"
	End If
	DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDScoreNow
	DMDFlush
	DMDScore
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
	if(dqTail <dqSize) Then
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
			For digit = 20 to 35
				DMDDisplayChar mid(dLine(0), digit-19, 1), digit
			Next
		Case 1 'bottom text line
			For digit = 0 to 19
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

Sub DMDInit
	Dim i
	'If Table1.ShowDT = true then
		Digits = Array(digit0, digit1, digit2, digit3, digit4, digit5, digit6, digit7, digit8, digit9, digit10, digit11,                  _
			digit12, digit13, digit14, digit15, digit16, digit17, digit18, digit19, digit20, digit21, digit22, digit23, digit24, digit25, _
			digit26, digit27, digit28, digit29, digit30, digit31, digit32, digit33, digit34, digit35)
		DigitsBack = Array(digit36)

	For i = 0 to 255:Chars(i)  = "dempty":Next '= "dempty":Images(i) = "dempty":Next

	Chars(32) = "dempty"
	'    Chars(34) = '"
	'    Chars(36) = '$
	'    Chars(39) = ''
	'    Chars(42) = '*
	'    Chars(43) = '+
	'    Chars(45) = '-
	'    Chars(47) = '/
	Chars(48) = "d0"       '0
	Chars(49) = "d1"       '1
	Chars(50) = "d2"       '2
	Chars(51) = "d3"       '3
	Chars(52) = "d4"       '4
	Chars(53) = "d5"       '5
	Chars(54) = "d6"       '6
	Chars(55) = "d7"       '7
	Chars(56) = "d8"       '8
	Chars(57) = "d9"       '9
	Chars(60) = "dless"    '<
	Chars(61) = "dequal"   '=
	Chars(62) = "dgreater" '>
	'   Chars(64) = '@
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
	'    Chars(94) = '^
	'    Chars(95) = '_
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
' Final State values are:   0=Off, 1=On, 2=Return to previous State
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
		If FinalState = 2 Then                      'Keep the current flasher state
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
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 1		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherOffBrightness = 0.5		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20)
Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
'InitFlasher 1, "green" : InitFlasher 2, "red" : InitFlasher 3, "white"
'InitFlasher 4, "green" : InitFlasher 5, "red" : InitFlasher 6, "white"
'InitFlasher 7, "green" : InitFlasher 8, "red"
InitFlasher 9, "blue" ': InitFlasher 10, "red" : InitFlasher 11, "white" 
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
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 60
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
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59)
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
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
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

' ***      script for demoing flashers					***
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
'   Table info & Attract Mode
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
	'   Put here your intro DMD

	DMD CL(0, "HIGHSCORES"), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
	DMD CL(0, "HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
	DMD CL(0, "HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
	DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
	DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
	DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
	DMD Space(dCharsPerLine(0) ), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode
	spinningwheel.enabled = 1
	ChangeSong
	StartLightSeq
	DMDFlush
	ShowTableInfo
End Sub

Sub StopAttractMode
	LightSeqAttract.StopPlay
	DMDScoreNow
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
'                     Table Specific Script Starts Here
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
End Sub

Sub StopEndOfBallMode()     'this sub is called after the last ball is drained
End Sub

Sub ResetNewBallVariables() 'reset variables for a new ball or player
Dim i
TargetBonus = 0
bBallSaverReady = True
End Sub

Sub ResetNewBallLights()    'turn on or off the needed lights before a new ball is released
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
	gi4.state = 0
End Sub

Sub TurnOffPlayfieldLights()
	Dim a
	For each a in aLights
		a.State = 0
	Next
End Sub

' *********************************************************************
'                        Table Object Hit Events
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
'    If li001.State=1 then 
'	AddScore 210
'	end if
'	If li002.State=1 then 
'	AddScore 420
'	end if
	PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
	RSling.Visible = 0:RSling1.Visible = 1
	sling1.rotx = 20
	RStep = 0
	RightSlingShot.TimerEnabled = 1
	AddScore 210
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
'	If li001.State=1 then 
'	AddScore 210
'	end if
'	If li002.State=1 then 
'	AddScore 420
'	end if
	PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
	LSling.Visible = 0:LSling1.Visible = 1
	sling2.rotx = 20
	 LStep = 0
	LeftSlingShot.TimerEnabled = 1
	AddScore 210
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
'triggers
'*****************

Sub Bonuschecker_Hit
'FlashForMs Flasher4, 1000, 50, 0
'FlashForMs Flasher5, 1000, 50, 0
'FlashForMs Flasher6, 1000, 50, 0
'FlashForMs Flasher7, 1000, 50, 0
'FlashForMs Flasher8, 1000, 50, 0
'FlashForMs Flasher9, 1000, 50, 0
'If li003.state=1 then
'DoraBonus = 1
'Else
'DoraBonus = 0
'end if
'If li004.state=1 then
'EmonBonus = 1
'Else
'EmonBonus = 0
'end if
'If li033.state=1 then
'DoraEmonBonus = 1
'Else
'DoraEmonBonus = 0
'end if
'If li015.state=1 then
'CatBonus = 1
'Else
'CatBonus = 0
'end if
'If li016.state=1 then
'HomeBonus = 1
'Else
'HomeBonus = 0
'end if
'If li034.state=1 then
'AnywhereBonus = 1
'Else
'AnywhereBonus = 0
'end if
'If li005.state=1 then
'RampBonus1 = 1
'Else
'RampBonus1 = 0
'end if
'If li006.state=1 then
'RampBonus2 = 1
'Else
'RampBonus2 = 0
'end if
'If li007.state=1 then
'RampBonus3 = 1
'Else
'RampBonus3 = 0
'end if
'If li017.state=1 then
'ALLRampBonus = 1
'Else
'ALLRampBonus = 0
'end if
'If li018.state=1 then
'MulitballBonus = 1
'Else
'MulitballBonus = 0
'end if
End Sub

'************************** 
'Bumpers 
'************************** 
Dim bumperHits

sub scorebumpers
'If li001.State=1 then 
'	AddScore 2500
'	bonusyscorechecker = bonusyscorechecker + 500
'	end if
'	If li002.State=1 then 
'	AddScore 5000
'	bonusyscorechecker = bonusyscorechecker + 1000
'	end if
PlaySound "shakegrog"
addscore 2500
end sub	

Sub Bumper001_hit()
ObjLevel(9) = 1 : FlasherFlash9_Timer
	PlaySound "BlockLo"
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
ObjLevel(9) = 1 : FlasherFlash9_Timer
	PlaySound "BlockLo"
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
ObjLevel(9) = 1 : FlasherFlash9_Timer
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


Sub CheckBumpers()
	If bumperHits <= 0 Then
		BumperBonus = BumperBonus + 1
		DMD "_", CL(1, "BUMPERS BONUS " & BumperBonus), "_", eNone, eBlink, eNone, 500, True, ""
		bumperHits = 100
	' do something more
	End If
End Sub

Sub ResetBumpers()
	bumperHits = 100
End Sub

'*****************
'* Maths
'*****************
Dim Pi
Pi = Round(4 * Atn(1), 6)
Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
	if ABS(dSin) < 0.000001 Then dSin = 0
	if ABS(dSin) > 0.999999 Then dSin = 1' * sgn(dSin)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
	if ABS(dCos) < 0.000001 Then dCos = 0
	if ABS(dCos) > 0.999999 Then dCos = 1' * sgn(dCos)
End Function

'*****************
'Targets
'*****************

'*****************
'Gates
'*****************
sub Gate_Hit()
'ObjLevel(1) = 1 : FlasherFlash9_Timer
	startB2S(3)
End Sub
'*****************
'Kickers
'*****************

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
'Flames2 = Array("magma16", "magma17", "magma18", "magma19", "magma20", "magma21", "magma22", "magma23", "magma24", "magma25", "magma26", "magma27", "magma28", "magma29", "magma30", "magma31")
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
Sub spinningwheel_Timer
'	countr = countr + 1 : If Countr > 15 then Countr = 1 : end If 
'	select case countr
'		case 1 : wheel1.RotZ=0
'		case 2 : wheel1.RotZ=24
'		case 3 : wheel1.RotZ=48
'		case 4 : wheel1.RotZ=72
'		case 5 : wheel1.RotZ=98
'		case 6 : wheel1.RotZ=122
'		case 7 : wheel1.RotZ=146
'		case 8 : wheel1.RotZ=168
'		case 9 : wheel1.RotZ=192
'		case 10 : wheel1.RotZ=216
'		case 11 : wheel1.RotZ=240
'		case 12 : wheel1.RotZ=264
'		case 13 : wheel1.RotZ=288
'		case 14 : wheel1.RotZ=312
'		case 15 : wheel1.RotZ=336
'	end Select
	Me.Interval = 1
	Wheel1.Rotz = Wheel1.Rotz + 0.24
	If Wheel1.Rotz >= 360 Then Wheel1.Rotz = 1
End Sub

'*****************
'smoke animation
'*****************
Sub smokeytimer_Timer
	countr1 = countr1 + 1 : If Countr1 > 18 then resetsmoke : end If 
	select case countr1
		case 1 : canonsmoke.visible = 1
		case 2 : canonsmoke.ImageA = "smoke1"
		case 3 : canonsmoke.ImageA = "smoke2"
		case 4 : canonsmoke.ImageA = "smoke3"
		case 5 : canonsmoke.ImageA = "smoke4"
		case 6 : canonsmoke.ImageA = "smoke5"
		case 7 : canonsmoke.ImageA = "smoke6"
		case 8 : canonsmoke.ImageA = "smoke7"
		case 9 : canonsmoke.ImageA = "smoke8"
		case 10 : canonsmoke.ImageA = "smoke9"
		case 11 : canonsmoke.ImageA = "smoke10"
		case 12 : canonsmoke.ImageA = "smoke11"
		case 13 : canonsmoke.ImageA = "smoke12"
		case 14 : canonsmoke.ImageA = "smoke13"
		case 15 : canonsmoke.ImageA = "smoke14"
		case 16 : canonsmoke.ImageA = "smoke15"
		case 17 : canonsmoke.ImageA = "smoke16"
		case 18 : canonsmoke.visible = 0
	end Select
End Sub


sub resetsmoke
smokeytimer.enabled = 0
Countr1 = 0
end sub
