Option Explicit
Randomize

Const Ballsize = 25
On Error Resume Next
ExecuteGlobal GetTextFile("core.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim AutoPlunger
Dim MouthDir:MouthDir = 1
Dim TempR, TempG, ColorDir, Sway
ColorDir = 1
TempR = 255
TempG = 255
Sway = 1

'***************** Table Initialization
Sub Table1_Init()
	AKick.enabled = 0
	EKick.enabled = 0
	OKick.enabled = 0
	EEKTrapper.IsDropped = 1
	OOKTrapper.IsDropped = 1
	ACKTrapper.IsDropped = 1
	StoreIntensity()
	Kicker10.CreateBall.Image = "blue ball"
	BallStopper.IsDropped = 1
	VooDooProt.IsDropped = 1
	For Each X in Magic
		X.X = 500
		X.Y = -70
	Next
	mAGNET2kicker.CreateBall.ID = 666
	KiMainTrough1.CreateBall
	KiMainTrough2.CreateBall
	KiMainTrough3.CreateBall
	Kickout.CreateBall
	Set AutoPlunger = New cvpmImpulseP
	With AutoPlunger
		.InitImpulseP TrAutoPlunge, 50, 1
		.CreateEvents "AutoPlunger"
	End With
	emAvalaible = True
'	TextBox1.Text= "": TextBox3.Text="F1 = HELP"
'	ScoreText.Text= ""
	MoveHead(180)
	Randomize
	FlLeftTrap.RotateToStart
	ResetMonkeys 'reset monkeys targs and lights
	MoveMouthguard(-1)
	DisplayInit 20,7,1
	DisplayQueueScreen "    TESTING     ", "    TESTING     ",3,3,0,1000,False,"Init5"
	DisplayQueueScreen "     TABLE      ", "       OK       ",8,8,0,1000,False,""
	DisplayQueueScreen "  MONKEY ISLAND ", "     "&VersionString&"     ",4,4,0,2000,False,""
	DisplayQueueScreen " COPYRIGHT 2005 ", "   IAN  TAYLOR  ",0,0,0,2000,False,""
	DisplayQueueScreen " VISUAL PINBALL ", " BY RANDY DAVIS ",0,0,0,1500,false,""
	DisplayQueueScreen "   VP DISPLAY   ", "    BY  BLACK   ",0,0,0,1500,false,""
	DisplayQueueScreen "    PRESS  6    ", "    FOR  MENU   ",3,3,0,2000,False,"clang"
	TextBox1.Text= ""

	Set ScummMag = New cvpmMagnet
	With ScummMag
		.InitMagnet ScummKicker, 50
		.GrabCenter = 1
		.X = ScummKicker.X
		.Y = ScummKicker.Y
		.Size = 100
	End With

	Set RovingMagnet = New cvpmMagnet 						'Magnet stuff
	With RovingMagnet
		.InitMagnet Magnet2a, 5
		.X = Magnet2Kicker.X
		.Y = Magnet2Kicker.Y
		.Size = 100
		.GrabCenter = 0
	End With

	Set voodoomag = New cvpmMagnet
	With voodoomag
		.InitMagnet Kicker6, 50
		.GrabCenter = 1
		.X = Kicker6.X
		.Y = Kicker6.Y
		.Size = 100
	End With

	Set MonkeyMag2 = New cvpmMagnet
	With MonkeyMag2
		.InitMagnet AKick, 50
		.GrabCenter = 1
		.X = AKick.X
		.Y = AKick.Y
		.Size = 100
	End With

	Set MonkeyMag1 = New cvpmMagnet
	With MonkeyMag1
		.InitMagnet EKick, 50
		.GrabCenter = 1
		.X = EKick.X
		.Y = EKick.Y
		.Size = 100
	End With

	Set MonkeyMag3 = New cvpmMagnet
	With MonkeyMag3
		.InitMagnet OKick, 50
		.GrabCenter = 1
		.X = OKick.X
		.Y = OKick.Y
		.Size = 100
	End With

'	TextBox1.Text= "Press 5 For Credit"
	TiLights.Enabled = 0
	Attract1.Play SeqCircleOutOn ,50,,2000
	ScummOn													'scumm bar interior lights
End Sub
'*****************

dim voodooball
Sub Magnet2a_Hit()
	RovingMagnet.AddBall ActiveBall
	If Status = "SwordMaster" Then
		RovingMagnet.X = ActiveBall.X
		RovingMagnet.Y = (VooDooSlope * RovingMagnet.X) + VDC
	End If
End Sub

Sub Magnet2a_UnHit()
	RovingMagnet.RemoveBall ActiveBall
End Sub

' ****************************** Keyboard Handlers *************************

Sub Table1_KeyDown(ByVal keycode)
	'Test Kicker Stuff
	If Keycode = 20 Then															'EP- T  "Pick up" the ball
		If NOT (BallMoverHold is Nothing) Then
			BallMoverHold.X = KiHold.X
			BallMoverHold.Y = KiHold.Y
		End If
	End If
	If Keycode = 30 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -45, 20, 0			'EP- A	Eek shot
	If Keycode = 31 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -35, 60, 0			'EP- S	Left Loop
	If Keycode = 18 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -26, 30, 0			'EP- E	"I" target
	If Keycode = 33 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -23, 40, 0			'EP- F	Scumm Shot
	If Keycode = 34 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -15, 25, 0			'EP- G	"N" target
	If Keycode = 35 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -12, 40, 0			'EP- H	Scumm Ramp
	If Keycode = 36 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -8, 25, 0			'EP- J	"S" target
	If Keycode = 37 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick -7, 40, 0			'EP- K	Le Chuck
	If Keycode = 38 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 1, 25, 0			'EP- L	Ack shot
	If Keycode = 39 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 7, 40, 0			'EP- ;	Monkey Head
	If Keycode = 40 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 15, 30, 0			'EP- '	Voodoo lady
	If Keycode = 44 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 18, 60, 0			'EP- Z	"U" target
	If Keycode = 45 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 22, 60, 0			'EP- X	Banana ramp
	If Keycode = 46 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 28, 60, 0			'EP- C	"L" target
	If Keycode = 47 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 31, 60, 0			'EP- V	Grog/right loop
	If Keycode = 48 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 38, 30, 0			'EP- B	"T" target
	If Keycode = 49 Then KiTest1.CreateBall.ID= 21:KiTest1.Kick 42, 30, 0			'EP- N	Ook shot
	'************** end test kickers***********
	If keycode = LeftTiltKey Then
		Nudge 90, 2
	End If
	If keycode = RightTiltKey Then
		Nudge 270, 2
	End If
	If keycode = CenterTiltKey Then
		Nudge 0, 2
	End If
	Select Case (keycode)
	Case PlungerKey
		If Plungeokay=1 Then
			AutoPlunger.AutoFire
			Bang
			DisplayscoreQueue 5,5
			Plungeokay=0
			KickoutReminderTimer.Enabled=False
		End If
		If (Status="HighDive") Then DiveProcessKey(keycode)
		If (Status="MonkeyBeat") Then BeatProcessKey(keycode)
		If (hsbModeActive = True) Then HighScoreProcessKey(keycode)
	Case LeftFlipperKey
		MoveHead(90)			'For Testing
		If emModeActive = True Then
			MenuIndex=MenuIndex+1
			OperatorMenu: PlaySound "Ack"
		ElseIf ModePicking = 1 Then
			PickMode(-1)
		Else
			If GameStarted=0 Then
				If (hsbModeActive = True) Then
					HighScoreProcessKey(keycode)
				End If
			Else 'in progress
				If Flipokay=1 			 Then LeftFlipper.RotateToEnd:PlaySound "fx_flipperup", 0, .67, -0.05, 0.05
				If (Status="Spitting") 	 Then SpitProcessKey(keycode)
				If (Status="HighDive") 	 Then DiveProcessKey(keycode)
				If (Status="MonkeyBeat") Then BeatProcessKey(keycode)
				If MysteryActive=True 	 Then MysteryProcessKey(keycode)
			End If
		End If
	Case RightFlipperKey
		MoveHead(-90)
		If emModeActive = True Then
			Select Case MenuIndex
				Case 1: MenuLineIndex(1)=MenuLineIndex(1)+1
				Case 2: MenuLineIndex(2)=MenuLineIndex(2)+1
				Case 3: MenuLineIndex(3)=MenuLineIndex(3)+1
				Case 4: MenuLineIndex(4)=MenuLineIndex(4)+1
			End Select
			OperatorMenu: PlaySound "tink"
		ElseIf ModePicking = 1 Then
			PickMode(1)
		Else
			If GameStarted=0 Then
				If (hsbModeActive = True) Then
					HighScoreProcessKey(keycode)
				End If
			Else
			 	If Flipokay=1 			 Then RightFlipper.RotateToEnd:PlaySound "fx_flipperup", 0, .67, 0.05, 0.05
				If (Status="Spitting") 	 Then SpitProcessKey(keycode)
				If (Status="HighDive") 	 Then DiveProcessKey(keycode)
				If (Status="MonkeyBeat") Then BeatProcessKey(keycode)
				If MysteryActive=True 	 Then MysteryProcessKey(keycode)
			End If
		End If
' >>> status report
'	Case 59	 ' "F1 Key"
'		If GameStarted=1 Then
'		DisplayQueueScreen "     STATUS     ","     REPORT    ",3,3,0,750,True,"init5"
'		Select Case Status
'		Case "MonkeyCombat"
'			Select Case CorrectMadHits
'			Case 0
'				DisplayQueueScreen " MONKEY  COMBAT ",""&SequenceToDo,0,0,0,10,False,""
'				DisplayQueueScreen " MONKEY  COMBAT ","    "&Mid(SequenceToDo,5,3)&"         ",0,11,0,1000,True,""
'			Case 1
'				DisplayQueueScreen " MONKEY  COMBAT ",""&SequenceToDo,0,0,0,10,False,""
'				DisplayQueueScreen " MONKEY  COMBAT ","        "&Mid(SequenceToDo,9,3)&"     ",0,11,0,1000,True,""
'			Case 2
'				DisplayQueueScreen " MONKEY  COMBAT ",""&SequenceToDo,0,0,0,10,False,""
'				DisplayQueueScreen " MONKEY  COMBAT ","            "&Mid(SequenceToDo,13,3)&" ",0,11,0,1000,True,""
'			Case 3
'			End Select
' 		Case "TreasureHunt"
'			Select Case TH_ToDo
'			Case 1
'				DisplayQueueScreen " SHOOT LEFT LOOP"," TWICE IN COMBO ",0,0,0,1500,True,""
'			Case 2
'				DisplayQueueScreen "    HIT  THE    ","BLINKING  MONKEY",0,0,0,1500,True,""
'			Case 3
'				DisplayQueueScreen "    HIT  THE    ","BLINKING  INSULT",0,0,0,1500,True,""
'			Case 4
'				DisplayQueueScreen "   GO  TO THE   "," HOUSE OF MOJO  ",0,0,0,1500,True,""
'			End Select
'		Case "VoodooMagic"
'			X=VoodooItemTodo-VoodooItem
'			DisplayQueueScreen "HIT CAPTIVE BALL","  " & x & " HITS TO GO ",0,0,0,1000,True,""
'		Case "Spitting","HighDive"
'			DisplayQueueScreen "   SHOOT  THE   ","  GROG MACHINE  ",0,0,0,2000,False,""
'			DisplayQueueScreen "    TO ENTER    ","   COMPETITION  ",0,0,0,2500,True,""
'		Case "SwordMaster"
'			DisplayQueueScreen "   HIT INSULT   ","     TARGETS    ",0,0,0,500,False,""
'			DisplayQueueScreen "  "&II&" "&NN&" "&SS&" "&UU&" "&LL&" "&TT&"   ","  SPELL INSULT  ",0,0,0,500,True,""
'		Case "DaintyLady"
'			DisplayQueueScreen "  HIT FLASHING  ","    LIGHT TO    ",0,0,0,5000,False,""
'			DisplayQueueScreen "    GET THE     ","      SHIP      ",0,0,0,500,True,""
'		Case "GraveDigger"
'			DisplayQueueScreen "  HIT FLASHING  ","     LIGHT      ",0,0,0,5000,True,""
'		Case "Normal"
'			If ModesStarted=8 Then
'				If EscapeItems=8 Then 'if all items collected give message to shoot lechuck
'					DisplayQueueScreen " ALL ITEMS FOUND"," SHOOT LECHUCK  ",3,3,0,2000,False,"init5"
'				Else
' 					DisplayQueueScreen "   ALL  MODES   ","    COMPLETE    ",3,3,0,1000,False,"init5"
'					DisplayQueueScreen "      SHOOT     ","     LECHUCK    ",8,8,0,1000,False,"chuck1"
'				End If
'			End If
'			If EBLight.State=LightStateBlinking Then
'				DisplayQueueScreen " EXTRA BALL LIT ","SHOOT SCUMM BAR",2,2,0,1000,false,""
'			Else
'				If ComboActive=1 Then
'					DisplayQueueScreen "  COMBO ACTIVE  ","SHOOT SCUMMLOOP",2,2,0,1000,false,""
'				Else
'					If GrogKicker.Enabled=True Then
'						DisplayQueueScreen "   SHOOT  THE   ","  GROG MACHINE  ",2,2,0,1000,false,""
'					Else
'						DisplayQueueScreen "  "&II&" "&NN&" "&SS&" "&UU&" "&LL&" "&TT&"   ","  SPELL INSULT  ",2,2,0,1000,false,""
'					End If
'				End If
'			End If
'			DisplayQueueScreen "      "&FormatScore1K(Barrels,"","")&"       "," BARRELS BUSTED ",2,2,0,1000,False,""
'			DisplayQueueScreen "  " &FormatScore1K(Combos,"","") & " COMBOS    ","  " &FormatScore(ComboValue) &"     ",2,2,0,1000,False,""
'			DisplayQueueScreen "     BONUS      ","      X "&BonusMultiplier&"       ",2,2,0,1000,False,""
'			DisplayQueueScreen "   REPLAY  AT   ","   " &FormatScore(gsReplayValue) & "    ",2,2,0,1000,false,""
'			DisplayScoreQueue 2,1
'		Case "Multiball"
'			If JPLitLight.State=LightStateBlinking Then
'				DisplayQueueScreen "    SHOOT THE   ","   SCUMM LOOP   ",0,0,0,1000,False,""
'				DisplayQueueScreen "    TO LITE     ","    JACKPOTS    ",3,3,0,1000,True,""
'			End If
'			If Super=1 Then DisplayQueueScreen " SHOOT LE CHUCK "," SUPER JACKPOT  ",3,3,0,1500,True,""
'			If JPLight.State=LightStateBlinking Then
'				DisplayQueueScreen "   SHOOT  THE   ","  MONKEY  HEAD  ",0,0,0,1000,False,""
'				DisplayQueueScreen "       FOR      ","     JACKPOT    ",0,0,0,50,False,""
'				DisplayQueueScreen "       FOR      ","     JACKPOT    ",3,3,0,1000,True,""
'			End If
'		Case Else
'			DisplayQueueScreen "  ESCAPE  MODE  "," HIT EVERYTHING ", 0,8,8,1000,True,""
'		End Select
'		End If
	Case StartGameKey
		If emModeActive = True Then
			OperatorMenuValidate
			PlaySound "tink"
		ElseIf ModePicking = 1 Then
			PickMode(0)
		Else
			If GameStarted=0 Then
				If (hsbModeActive = True) Then
					HighScoreProcessKey(keycode)
				Else
					If Credits>0 Then
						Credits=Credits-1
						GameStarted=1
						StartGame
					Else
						DisplayFlushQueue
						DisplayQueueScreen "     INSERT     ","      COINS     ",3,3,0,500,False,""
						TextBox1.Text= "Press 5 For Credit"
					End If
				End If
			Else 'game in pregress
				If (Status="HighDive") Then
					DiveProcessKey(keycode)
				End If
			End If
		End If
	Case AddCreditKey
		If emModeActive = False Then
			emAvalaible = False
			Credits=Credits+1
			Backflash
			DisplayFlushQueue
			PlaySound "start"
			If GameStarted=0 Then
				If (hsbModeActive = True) Then
					HighScoreProcessKey(keycode)
				Else
					If Credits=0 Then
						DisplayQueueScreen "     INSERT     ","      COINS     ",3,3,0,1000,False,""
					Else
						DisplayQueueScreen "   CREDITS "&Credits&"    ","  PRESS  START  ",3,3,0,1000,False,""
					End If
					TextBox1.Text= "Press 1 To Start"
					PlaySound "quarter"
					TiLights.Enabled = 0
					StopSeqs()
					Attract1.Play SeqCircleInOff ,50,,2000
				End If
			Else
				If Credits=0 Then
					DisplayQueueScreen "     INSERT     ","      COINS     ",3,3,0,1000,False,""
				Else
					DisplayQueueScreen "   CREDITS "&Credits&"    ","  PRESS  START  ",3,3,0,1000,False,""
				End If
			DisplayScoreQueue 2,1
			End If
		End If
	Case 7
		If emAvalaible = True Then
			Lightning()
			Backflash()
			If emModeActive = False Then
				emModeActive = True
				Reset_HS="NO"
				InAttract = 0
				DisplayFlushQueue
				DisplayQueueScreen " COIN DOOR OPEN","   CONFIG MODE  ",3,0,0,1000,False,"flapopen"
				DisplayQueueScreen "  PLAYFIED  HI "," POWER DISABLED ",8,8,0,1000,False,"init5"
				MenuIndex=1: MenuLineIndex(1)=1: MenuLineIndex(2)=1: MenuLineIndex(3)=1: MenuLineIndex(4)=1
				OperatorMenuTimer.Enabled=True
			Else
				emModeActive = False
				emAvalaible = False
				DisplayFlushQueue
				DisplayQueueScreen "COIN DOOR CLOSE"," EXIT CONF MODE ",3,0,0,1500,False,"flapclos"
				OperatorMenu_End
			End If
		End If
	End Select
End Sub

Sub Table1_KeyUp(ByVal keycode)'*********** KEY DOWN
	Select Case(keycode)
	Case LeftFlipperKey
		PlaySound "fx_flipperdown", 0, 1, -0.05, 0.05
		LeftFlipper.RotateToStart
	Case RightFlipperKey
		RightFlipper.RotateToStart
		PlaySound "fx_flipperdown", 0, 1, 0.05, 0.05
	End Select
End Sub

'************** End Keyboard stuff

Dim ballmoverhold
Sub TrTester_Hit()
	Set ballmoverhold = ActiveBall
End Sub
For Each X in Magic
	X.IntensityScale = 1
Next

Sub Timer1_Timer()
	'Palm Trees
	'TextBox2.Text = PrTrunk1.RotY
	PrTrunk1.RotY = dSin(Sway)*2
	PrTrunk2.RotY = PrTrunk1.RotY
	PrTrunk3.RotY = PrTrunk1.RotY
	PrTrunk4.RotY = PrTrunk1.RotY
	PrLeaves1.RotY = PrTrunk1.RotY
	PrLeaves2.RotY = PrTrunk1.RotY
	PrLeaves3.RotY = PrTrunk1.RotY
	PrLeaves4.RotY = PrTrunk1.RotY
	Sway = Sway + 0.05
	If Sway > 360 Then Sway = 1
	'******
	'Le chuck's Beard matches the gate
	PrLeChuckBeard.RotX = -Gate2.CurrentAngle
	'******
	'Rotate and move the magic dust
	For Each X in Magic
		X.RotZ = X.RotZ + 1
		X.Height = X.Height + 0.025
		If X.Height >= 80 Then
			X.Height = 25
			If MotorTimer.Enabled = True Then
				X.X = RovingMagnet.X
				X.Y = RovingMagnet.Y
			End If
		End If
	Next
	'***********
	'Temp Mouth testing

	'***********
End Sub

Dim Waterfall:Waterfall = Array("Frame 10 (100ms)", "Frame 11 (100ms)", "Frame 12 (100ms)", "Frame 13 (100ms)", "Frame 14 (100ms)", "Frame 15 (100ms)", "Frame 16 (100ms)", "Frame 17 (100ms)", "Frame 18 (100ms)", "Frame 19 (100ms)", "Frame 20 (100ms)", "Frame 21 (100ms)", "Frame 22 (100ms)", "Frame 23 (100ms)", "Frame 24 (100ms)", "Frame 25 (100ms)", "Frame 26 (100ms)", "Frame 27 (100ms)", "Frame 28 (100ms)", "Frame 29 (100ms)", "Frame 30 (100ms)", "Frame 31 (100ms)", "Frame 32 (100ms)", "Frame 33 (100ms)", "Frame 34 (100ms)", "Frame 35 (100ms)", "Frame 36 (100ms)", "Frame 37 (100ms)", "Frame 38 (100ms)", "Frame 39 (100ms)", "Frame 40 (100ms)")
Dim Waterfallkey:WaterfallKey = 0
Sub TiWaterfall_Timer()
	Ramp12.Image = WaterFall(WaterfallKey)
	'Ramp11.Image = WaterFall(WaterfallKey)
	'Ramp10.Image = WaterFall(WaterfallKey)
	WaterfallKey = WaterfallKey + 1
	If WaterfallKey > 30 Then WaterfallKey = 1
End Sub

' *************************************************************************************
' le chuck trigger

Sub GetEscapeItems
	'EP- Head lighting effect
	PlaySound "chuck1"
	If EscapeItem1=1 Then
		DisplayQueueScreen "     FAMILY     ","   HEIRLOOMS    ",0,0,0,300,False,"escape2" 'voodoo
		AddScore (50000)
		Kicker3Delay=Kicker3Delay+300
	End If
	If EscapeItem2=1 Then
		DisplayQueueScreen "   MONKEY MUG   ","   MONKEY MUG   ",0,0,0,300,False,"escape2" 'spitting
		AddScore (50000)
		Kicker3Delay=Kicker3Delay+300
	End If
	If EscapeItem3=1 Then
		DisplayQueueScreen " BANANA PICKER  "," BANANA PICKER  ",0,0,0,300,False,"escape2" 'high dive
		AddScore (50000)
		Kicker3Delay=Kicker3Delay+300
	End If
	If EscapeItem4=1 Then
		DisplayQueueScreen " GUBERNATORIAL  ","      SEAL      ",0,0,0,300,False,"escape2" 'sword master
		AddScore (50000)
		Kicker3Delay=Kicker3Delay+300
	End If
	If EscapeItem5=1 Then
		DisplayQueueScreen "      MAP       ","      MAP       ",0,0,0,300,False,"escape2" 'dainty lady
		AddScore (50000)
		Kicker3Delay=Kicker3Delay+300
	End If
	If EscapeItem6=1 Then
		DisplayQueueScreen "   BRONZE HAT   ","   BRONZE HAT   ",0,0,0,300,False,"escape2" 'monkey combat
		AddScore (50000)
		Kicker3Delay=Kicker3Delay+300
	End If
	If EscapeItem7=1 Then
		DisplayQueueScreen "    HORDE OF    ","      GOLD      ",0,0,0,300,False,"escape2" 'grave digger
		AddScore (50000)
		Kicker3Delay=Kicker3Delay+300
	End If
	If EscapeItem8=1 Then
		DisplayQueueScreen "     CORPSE     ","     CORPSE     ",0,0,0,300,False,"escape2" 'treasure hunt
		AddScore (50000)
		Kicker3Delay=Kicker3Delay+300
	End If
	If EscapeItems<8 Then
		If ModesStarted=8 Then
			DisplayQueueScreen "   ALL MODES    ","   COMPLETED    ",3,3,0,1250,True,"init5"
			InitEscapeTimer.Enabled=True
		Else
			EndMusic()
			DisplayQueueScreen "   AAAARRRRRR   ","   AAAARRRRRR   ",8,8,0,500,False,""
			DisplayQueueScreen " COMPLETE MODES ","   FOR ITEMS    ",3,3,0,1250,True,""
			Kicker3.TimerInterval=Kicker3Delay
			Kicker3.TimerEnabled=True 'do not enable this timer when escape mode lit
		End If
	Else 'blink all items and start escape mode
		EndMusic()
		DisplayQueueScreen "   ALL ITEMS    ","   RECOVERED    ",3,3,0,1250,True,"init5"
		LightArray(31, 1) = 0
		LightArray(53, 1) = 0
		LightArray(4, 1) = 2
		LightArray(14, 1) = 2
		LightArray(26, 1) = 2
		LightArray(54, 1) = 2
		LightArray(7, 1) = 2
		LightArray(27, 1) = 2
		InitEscapeTimer.Enabled=True
		Kicker3Delay=0
	End If
	If EscapeItems>0 Then EndMusic()
End Sub

Dim LeChuckMover
Sub Kicker3_Hit()
	Set LeChuckMover = ActiveBall
	ActiveMonkeyBeat()
	If LightArray(51, 1) = 1 OR LightArray(51, 1) = 2 OR LightArray(51, 1) = 3 Then
		PlaySound "wallop"
		PlaySound "ballin"
		GoMonkeyBeat()
		LightArray(51, 1) = 0
		LightArray(31, 1) = 0
	Else
		DoChuck()
	End If
End Sub

Sub DoChuck()
	LightArray(49, 1) = 0
	PlaySound "wallop"
	PlaySound "ballin"
	HurryUpTimer.Enabled=False
	If Tilt=1 Then
		Kicker3.TimerEnabled=True
		Exit Sub
	End If
	Select Case Status
	Case "Normal"
		EndCurrentInsult()
		DisplayFlushQueue()
		AddBonus(2500)
		GetEscapeItems()
	Case "Multiball" 'Super Jackpot
	If Super=1 Then
		BallStopper.IsDropped=False
		TroughTimer.Enabled=True
		BallStop=1
		SuperJackpot()
	Else
		DisplayFlushQueue()
		DisplayQueueScreen " ADD TO JACKPOT ","      50K      ",0,0,0,500,True,""
		PlaySound "clack"
		PlaySound "multihit1"
		Jackpot=Jackpot+50000
		Kicker3Hits = Kicker3Hits+1
		CheckKicker3Timer.Enabled=True
	End If
	'modes
	Case "VoodooMagic"
	If HurryUp=1 Then
		EndMusic()
		PlaySound "bigstep"
		EscapeItem1=1
		EscapeItems=EscapeItems+1
		DisplayFlushQueue()
		DisplayQueueScreen "    GIFT FROM   ","   VOODOO LADY  ",6,7,0,1500,False,"iamvoodoo"
		DisplayQueueScreen "ITEM RECEIVED = ","    HEIRLOOMS   ",3,8,0,2500,False,"coins"
		ModeTotal=ModeTotal+ModeTotalCoeff
		ModeCompleteTimer.Enabled=True
		LightArray(54, 1) = 1
	Else
		Kicker3Hits = Kicker3Hits+1
		CheckKicker3Timer.Enabled=True
	End If
	Case "Spitting"
	If HurryUp=1 Then
		If NextSpitDistance=SpittingDistance(5) Then
			EndMusic()
			PlaySound "bigstep"
			EscapeItem2=1
			EscapeItems=EscapeItems+1
			DisplayFlushQueue
			DisplayQueueScreen "     SUPER      ","    SPITTER     ",6,7,0,1500,False,"splat"
			DisplayQueueScreen " ITEM RECEIVED= ","   MONKEY MUG   ",3,8,0,2500,False,"monkeymug"
			ModeTotal=ModeTotal+ModeTotalCoeff
			ModeCompleteTimer.Enabled=True
			LightArray(13, 1) = 1
		Else
			Kicker3.TimerEnabled=True
		End If
	Else
		Kicker3.TimerEnabled=True
	End If
	Case "HighDive"
	If HurryUp=1 Then
		If DiveNumber=DiveToDo Then
			EndMusic()
			PlaySound "bigstep"
			EscapeItem3=1
			EscapeItems=EscapeItems+1
			DisplayFlushQueue()
			DisplayQueueScreen "   THE JUDGES   ","  ARE IMPRESSED ",0,0,0,1500,False,"cheer1"
			DisplayQueueScreen " ITEM RECEIVED= "," BANANA  PICKER ",3,8,0,2500,False,"picker"
			PlaySound "haveabanana"
			LightArray(47, 1) = 1
			BananaPicker=1
			ModeTotal=ModeTotal+ModeTotalCoeff
			ModeCompleteTimer.Enabled=True
		Else
			Kicker3.TimerEnabled=True
		End If
	Else
		Kicker3.TimerEnabled=True
	End If
	Case "SwordMaster"
	If HurryUp=1 Then
		If SwordNumber=SwordToDo Then
			EndMusic()
			PlaySound "bigstep"
			EscapeItem4=1
			EscapeItems=EscapeItems+1
			DisplayFlushQueue()
'			PlaySound "iwon"
			DisplayQueueScreen "   SWORDMASTER  ","    DEFEATED    ",8,8,0,1500,False,"fairandsquare"
			DisplayQueueScreen " ITEM RECEIVED= "," SEAL OF MELEE  ",3,8,0,2500,False,"symbol"
			ModeCompleteTimer.Enabled=True
			ModeTotal=ModeTotal+ModeTotalCoeff
			LightArray(4, 1) = 1
		Else
			Kicker3.TimerEnabled=True
		End If
	Else
		Kicker3.TimerEnabled=True
	End If
	Case "DaintyLady"
	If HurryUp=1 Then
		If ShipNumber>3 Then
			EndMusic()
			PlaySound "bigstep"
			EscapeItem5=1
			EscapeItems=EscapeItems+1
			DisplayFlushQueue()
			DisplayQueueScreen "  SET SAIL TO   ","  MONKEY ISLAND ",3,3,0,1500,False,"ahmi"
			DisplayQueueScreen " ITEM RECEIVED= ","       MAP      ",3,8,0,2500,False,"ivecometomap"
			ModeCompleteTimer.Enabled=True
			ModeTotal=ModeTotal+ModeTotalCoeff
			LightArray(7, 1) = 1
		Else
			Kicker3.TimerEnabled=True
		End If
	Else
		Kicker3.TimerEnabled=True
	End If
	Case "MonkeyCombat"
	If HurryUp=1 Then
		If CorrectMadSet=MadCombatToDo Then
			EndMusic()
			PlaySound "bigstep"
			EscapeItem6=1
			EscapeItems=EscapeItems+1
			DisplayFlushQueue()
			DisplayQueueScreen " MEGA LE CHUCK  ","    DEFEATED    ",8,8,0,1500,False,"dargh"
			DisplayQueueScreen " ITEM RECEIVED= ","   BRONZE HAT   ",0,0,0,2500,False,"bronzehat"
			ModeCompleteTimer.Enabled=True
			ModeTotal=ModeTotal+ModeTotalCoeff
		Else
			Kicker3.TimerEnabled=True
		End If
	Else
		Kicker3.TimerEnabled=True
	End If
	Case "TreasureHunt"
	If HurryUp=1 Then
		EndMusic()
		PlaySound "hey"
		EscapeItem7=1
		EscapeItems=EscapeItems+1
		DisplayFlushQueue()
		DisplayQueueScreen "   GOLD HORDE   ","      FOUND     ",3,3,0,1500,False,"thatslechucksproperty"
		DisplayQueueScreen " ITEM RECEIVED= ","      GOLD      ",3,8,0,2500,False,"coins"
		ModeCompleteTimer.Enabled=True
		ModeTotal=ModeTotal+ModeTotalCoeff
		LightArray(26, 1) = 1
	Else
		Kicker3.TimerEnabled=True
	End If
	Case "GraveDigger"
	If HurryUp=1 Then
		If GraveTarget>4 Then
			EndMusic()
			PlaySound "bigstep"
			EscapeItem8=1
			EscapeItems=EscapeItems+1
			DisplayFlushQueue()
			DisplayQueueScreen "     GRAVE      ","     DIGGER     ",3,3,0,1500,False,"getout"
			DisplayQueueScreen " ITEM RECEIVED= ","     CORPSE     ",3,8,0,2500,False,"voodoohit"
			ModeCompleteTimer.Enabled=True
			ModeTotal=ModeTotal+ModeTotalCoeff
			LightArray(27, 1) = 1
		Else
			Kicker3.TimerEnabled=True
		End If
	Else
		If GraveTarget = 3 Then GotGraveTarget
		Kicker3.TimerEnabled=True
	End If
	Case "GoEscape"
		If EscapeSwitch(17)=0 Then
			LightArray(31, 1) = 1
			SwitchNumber=17
			SwitchOn()
		End If
		Kicker3Hits = Kicker3Hits+1
		CheckKicker3Timer.Enabled=True
	End Select
	HurryUp=0
	LightArray(31, 1) = 1			'EP- But... it just got turned on possibly??
End Sub

Sub ModeCompleteTimer_Timer()	' to allow displays before GoModeTotal...
	ModeCompleteTimer.Enabled=False
	ModeComplete=1
	GoModeTotal()
	Kicker3.TimerEnabled=True
End Sub

Sub CheckKicker3Timer_Timer()
	If Kicker3.TimerEnabled=True Then Exit Sub
	Kicker3.TimerEnabled=True
	Kicker3Hits = Kicker3Hits-1
	If Kicker3Hits<1 Then CheckKicker3Timer.Enabled=False
End Sub

Sub Kicker3_Timer()
	Me.TimerEnabled = 0
	LeChuckMover.X = Kicker11.X
	LeChuckMover.Y = Kicker11.Y
	LeChuckMover.Z = 370
	Me.Kick 270, 2, 0
	PlaySound "SoloOn"

	Kicker3.TimerEnabled=False
	LightArray(50, 1) = 0
	LightArray(31, 1) = 0
	If Status = "SwordMaster" Or Status="TreasureHunt" Then
		Set BallSM = LeChuckMover
	End If
	Me.Kick 270, 2, 0
	PlaySound "ballinr"
	PlaySound "ballroll2"
	If Status="Normal" Then
		Song="EFMI_MonkeyTheme.mp3": PlayMusic Song
		Kicker3Delay=2000
	End If

	Set LeChuckMover = Nothing
End Sub

Sub Kicker11_Timer() 'used for delay when kicking out in other modes - turns bumper light off
	Kicker11.TimerEnabled=False
	'EP- Stop lighting effect if you haven't already
	Kicker11.Kick 270,5
	GrogKicker.Kick 270,5
	PlaySound "ballinr": PlaySound "ballroll2"
	If Tilt=1 Then Exit Sub
	If Status = "HighDive" Then
		Dive=0
		If DiveNumber < DiveToDo Then ModeLengthTimer.Enabled=True
	End If
	If Status = "Spitting" Then
	Spitting=0
		If SpitNumber<5 Then ModeLengthTimer.Enabled=True
	End If
End Sub

'*******************************
'*		Bumper Animations
'*******************************

'---------------------- barrels
Sub BustBarrels()
	Barrels=Barrels+1
	If Status <> "GoEscape" Then
		DisplayFlushQueue
		DisplayQueueScreen "      "&Barrels&"       "," BARRELS BUSTED ",0,0,0,100,False,""
	End If
	AddScore(50000): AddBonus(1000)
	PlaySound "shakegrog"
End Sub

Sub Bumper1_Hit()
	AdvanceModeCount()
	PlaySound "BlockHi"
	Me.TimerEnabled = 1
	TipFull1 = 8
	HasJumped1 = 0
	If Status="GoEscape" Then
		If EscapeSwitch(10)=0 Then
			Bumper4.State=LightStateOn
			SwitchNumber=10
			SwitchOn
		End If
	Else
		BustBarrels
	End If
End Sub

Dim Tip1, TipFull1, Jump1, HasJumped1, JumpSpeed
Jumpspeed = 1
Jump1 = 1
Tip1 = 1
TipFull1 = 8
Sub Bumper1_Timer()
	If HasJumped1 = 0 Then
		PrBarrel1.Z = dSin(Jump1)*30 + 100
		Jump1 = Jump1 + 1
		If Jump1 >= 180 Then
			Jump1 = 1
			HasJumped1 = 1
		End If
	End If
	If HasJumped1 = 1 Then
		PrBarrel1.RotY = dSin(Tip1)*TipFull1
		PrBarrel1.ObjRotZ = PrBarrel1.ObjRotZ + 0.1
		If PrBarrel1.ObjRotZ >= 360 Then PrBarrel1.ObjRotZ = 0
		Tip1 = Tip1 + 1
		If Tip1 > 360 Then
			Tip1 = 1
			TipFull1 = TipFull1 - 1
		End If
		If TipFull1 <= 0 Then Me.TimerEnabled = 0
	End If
End Sub

Sub Bumper2_Hit()
	AdvanceModeCount()
	PlaySound "BlockMid"
	Me.TimerEnabled = 1
	TipFull2 = 8
	HasJumped2 = 0
	If Status="GoEscape"  Then
		If EscapeSwitch(11)=0 Then
			Bumper3.State=LightStateOn
			SwitchNumber=11: SwitchOn
		End If
	Else
		BustBarrels
	End If
End Sub

Dim Tip2, TipFull2, Jump2, HasJumped2
Jump2 = 1
Tip2 = 1
TipFull2 = 8
Sub Bumper2_Timer()
	If HasJumped2 = 0 Then
		PrBarrel2.Z = dSin(Jump2)*30 + 100
		Jump2 = Jump2 + 1
		If Jump2 >= 180 Then
			Jump2 = 1
			HasJumped2 = 1
		End If
	End If
	If HasJumped2 = 1 Then
		PrBarrel2.RotY = dSin(Tip2)*TipFull2
		PrBarrel2.ObjRotZ = PrBarrel2.ObjRotZ + 0.1
		If PrBarrel2.ObjRotZ >= 360 Then PrBarrel2.ObjRotZ = 0
		Tip2 = Tip2 + 1
		If Tip2 > 360 Then
			Tip2 = 1
			TipFull2 = TipFull2 - 1
		End If
		If TipFull2 <= 0 Then Me.TimerEnabled = 0
	End If
End Sub

Sub Bumper3_Hit()
	AdvanceModeCount()
	PlaySound "BlockLo"
	Me.TimerEnabled = 1
	TipFull3 = 8
	HasJumped3 = 0
	If Status="GoEscape"  Then
		If EscapeSwitch(12)=0 Then
			SwitchNumber=12: SwitchOn
			Bumper1.State=LightStateOn
		End If
	Else
		BustBarrels
	End If
End Sub

Dim Tip3, TipFull3, Jump3, HasJumped3
Jump3 = 1
Tip3 = 1
TipFull3 = 8
Sub Bumper3_Timer()
	If HasJumped3 = 0 Then
		PrBarrel3.Z = dSin(Jump3)*30 + 100
		Jump3 = Jump3 + 1
		If Jump3 >= 180 Then
			Jump3 = 1
			HasJumped3 = 1
		End If
	End If
	If HasJumped3 = 1 Then
		PrBarrel3.RotY = dSin(Tip3)*TipFull3
		PrBarrel3.ObjRotZ = PrBarrel3.ObjRotZ + 0.1
		If PrBarrel3.ObjRotZ >= 360 Then PrBarrel3.ObjRotZ = 0
		Tip3 = Tip3 + 1
		If Tip3 > 360 Then
			Tip3 = 1
			TipFull3 = TipFull3 - 1
		End If
		If TipFull3 <= 0 Then Me.TimerEnabled = 0
	End If
End Sub
'******** End Bumper Animations************

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

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Dim SoundLevelMult:SoundLevelMult = 1

Function LVL(input) : LVL = Input * SoundLevelMult : End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
Dim tmp
tmp = ball.x * 2 / table1.width-1
If tmp > 0 Then
	Pan = Csng(tmp ^10)
Else
	Pan = Csng(-((- tmp) ^10) )
End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 5 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
Dim i
For i = 0 to tnob
	rolling(i) = False
Next
End Sub

Sub RollingTimer_Timer()
Dim BOT, b
BOT = GetBalls

	' stop the sound of deleted balls
For b = UBound(BOT) + 1 to tnob
	rolling(b) = False
	StopSound("fx_ballrolling" & b)
Next

	' exit the sub if no balls on the table
If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
For b = 0 to UBound(BOT)
	If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
		rolling(b) = True
		PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
	Else
		If rolling(b) = True Then
			StopSound("fx_ballrolling" & b)
			rolling(b) = False
		End If
	End If
Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub



'************************************
' What you need to add to your table
'************************************

' a timer called RollingTimer. With a fast interval, like 10
' one collision sound, in this script is called fx_collide
' as many sound files as max number of balls, with names ending with 0, 1, 2, 3, etc
' for ex. as used in this script: fx_ballrolling0, fx_ballrolling1, fx_ballrolling2, fx_ballrolling3, etc


'******************************************
' Explanation of the rolling sound routine
'******************************************

' sounds are played based on the ball speed and position

' the routine checks first for deleted balls and stops the rolling sound.

' The For loop goes through all the balls on the table and checks for the ball speed and
' if the ball is on the table (height lower than 30) then then it plays the sound
' otherwise the sound is stopped, like when the ball has stopped or is on a ramp or flying.

' The sound is played using the VOL, PAN and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the PAN function will change the stereo position according
' to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner",0,.25,0,0.25
End Sub

Sub Rubbers_Hit(idx)
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 20 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
		RandomSoundRubber()
	End If
End Sub

Sub Posts_Hit(idx)
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		RandomSoundRubber()
	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

'******************************************************************************************************************************************
'* Original code that's been edited
'******************************************************************************************************************************************

Const VersionString ="V 1 RC"
Const SwingLimit=4 'tilt sensitivity
Const Freeplay=0 '1=on

'Dimension all Variables

Dim BallsPerGame
Dim Kicker3Hits
Dim BallSM
Dim CoeffTimer
Dim ModeTotalCoeff
Dim CoeffGoEscape
Dim SpitCoeff
Dim Super 'super jackpot
Dim Song
Dim ReplayGiven
Dim Tilt
Dim SaveActive 'lower save targets
Dim Barrels, BarrelScore
Dim EscapeItems, EscapeTotal, EscapeActive
Dim ModesGet(8)
Dim ModeCount, ModesStarted, ModeTotal, ModeTimeLeft, MoreTimeDelay
Dim GameStarted, GamesPlayed
Dim BallsInPlay, Ball, BallSave, BallsRemaining, LockedBalls, BallStop, BallSaved
Dim Light 'for lights collection
Dim Kicker3Delay: Kicker3Delay=0
Dim LBounce,MBounce,RBounce 'monkey bounce count
Dim ComboActive, Combos, ComboValue
Dim GrogHits, GrogTotal
Dim SpinningDirection
Dim MouthPos, HeadPos, HeadHits
Dim CombatAward, CombatLevel, CombatCount
Dim Combat1Count, Combat2Count, Combat3Count
Dim VoodooCount, VooDooItem, VoodooItemToDo
Dim Mystery, MysteryActive ' mystery key lock
Dim II, NN, SS, UU, LL, TT
Dim LastInsult, TH_Insult
Dim Multiball, MultiballEnded
Dim Jackpot
Dim ExtraBalls 'keeps count how how many extra balls you've scored.
Dim Bonus, BonusHeld, BonusMultiplier
Dim ComboMult
Dim HurryUp, HurryUpBonus, MaxHurryUp
Dim MB 'bonus multiplier
Dim PF 'playfield multiplier
Dim EB, EBScoredThisBall
Dim Score, Points, BPoints 'bonus points
Dim Credits
Dim x 'used for loops
Dim PFMultiplier
Dim Bob 'tilt mechanism
Dim SwingForce, SwingTotal
Dim Status 'table mode
Dim Flipokay, Plungeokay, NudgeOkay, NudgeAllowed
Dim RovingMagnet, MagnetsOn, MotorDirection, VoodooMag, MonkeyMag1, MonkeyMag2, MonkeyMag3, ScummMag, GrogMag
Dim MouthHits
Dim Spitting, SpitDistance, SpitPower, SpitPos(13) 'set in frames
Dim SpitPic, SpitCount, SpitActive, NextSpitKey, NextSpitDistance, SpitAdjust
Dim SpittingDistance(5)
Dim SpitNumber
Dim Dive, DiveNumber, DivesPerformed, DiveActive, NextDiveUpper, NextDiveLower
Dim DiveRangeStart, DiveRangeEnd, DiveToDo
Dim JudgeScore(3), CoeffDive(6), ScoreMini(6), ScoreMaxi(6)
' CoeefDive= maximum note per judge
' ScoreMini-ScoreMaxi: total mini and maxi for the 3 judges
CoeffDive(1)=2: CoeffDive(2)=4: CoeffDive(3)=6:  CoeffDive(4)=8:  CoeffDive(5)=9:  CoeffDive(6)=10
ScoreMini(1)=3: ScoreMini(2)=7: ScoreMini(3)=13: ScoreMini(4)=18: ScoreMini(5)=24: ScoreMini(6)=0
ScoreMaxi(1)=5: ScoreMaxi(2)=9: ScoreMaxi(3)=15: ScoreMaxi(4)=20: ScoreMaxi(5)=26: ScoreMaxi(6)=0
Dim TempDiveKey, DiveKeyNumber, DiveKeyLimit
Dim NextDSNumber
Dim TH_ToDo, TreasureHuntScores(4)
Dim LoopEntrance 'outside loop
Dim ScummLoops, NextScummAward
Dim Bananas, BananaPicker
Dim CorrectDiveCount
Dim SwordTarget, SwordNumber, SwordComplete, SwordToDo
Dim DaintyLadyScores(3)
Dim ShipNumber, ShipTarget, LastShipTarget
Dim MadSetNumber, MadNumber, MadTargetNumber, MadTarget(3)
Dim CorrectMadHits, CorrectMadSet, MadAllowed, WhatMadTarget
Dim MadCombatToDo
Dim GraveDiggerScores(4)
Dim GraveTarget, GraveTarget1, GraveTarget2, GraveTarget3
Dim SequenceToDo
Dim EscapeItem1, EscapeItem2, EscapeItem3, EscapeItem4, EscapeItem5, EscapeItem6, EscapeItem7, EscapeItem8
Dim EscapeSwitch(26), SwitchNumber, EscapeNb, GoComplete, ModeComplete
Dim BeatNb, BeatPos, BeatActive, MonkeyBeat 'monkey beat mode
Dim BeatTarget(305), MyBeatTarget(305), CorrectBeatTargets, CorrectBeatRounds, TotalCorrectBeats, Rounds

'Dive stuff

Dim TempDS(9)
For x=1 To 9: TempDS(x)="": Next
Dim DS(9)
Dim NextDiveNumber

'display animations
SpitPos(1)  = "            MAX "
SpitPos(2)  = "*           MAX "
SpitPos(3)  = "**          MAX "
SpitPos(4)  = "***         MAX "
SpitPos(5)  = "****        MAX "
SpitPos(6)  = "*****       MAX "
SpitPos(7)  = "******      MAX "
SpitPos(8)  = "*******     MAX "
SpitPos(9)  = "********    MAX "
SpitPos(10) = "*********   MAX "
SpitPos(11) = "**********  MAX "
SpitPos(12) = "*********** MAX "
SpitPos(13) = "************MAX "

'Game Stats and Operatot Menu
Dim emModeActive, emAvalaible
Dim Value, Option_Set
Dim Play_Mode, Reset_HS
Dim MenuIndex, MenuLineIndex(4)
Dim MaxLoopsThisGame
Dim gsLoopChamp, gsLoopChampName, gsHighScore(4), gsHighScoreName(4)
Dim gsReplayValue, gsLastScore, gsGamesPlayed

' Assign values to global and init variables
SpinningDirection=0 'head stopped
ReplayGiven=0: GameStarted=0: Plungeokay=0: GamesPlayed=0
Credits=0: Flipokay=0: HeadPos=15 'starts from right + moves to left
MouthPos=1 'closed
SwingForce=0: SwingTotal=0: Tilt=0
Jackpot=5000000
LoadStats

'******************************** Attract Mode *****************************
Dim InAttract				'Are we in attract Mode
InAttract = 0
Sub TiTesting_Timer()		'Once the "testing" completes, start regular attract mode stuff
	Me.Enabled = 0
	InAttract = 1
	AttractMode()
End Sub

Sub AttractMode()
	DisplayFlushQueue
	DisplayQueueScreen "  ESCAPE  FROM  ","                ",2,2,0,1000,False,""
	DisplayQueueScreen "  ESCAPE  FROM  "," MONKEY  ISLAND ",0,4,0,1000,False,""
	DisplayQueueScreen "  ESCAPE  FROM  "," MONKEY  ISLAND ",8,8,0,1500,False,""
	If Credits=0 Then
		DisplayQueueScreen "     INSERT     ","      COINS     ",3,3,0,1500,False,""
	Else
		DisplayQueueScreen "   CREDITS "&Credits&"    ","  PRESS  START  ",3,3,0,1500,False,""
	End If
	TextBox1.Text= "Press 5 For Credit"
	DisplayQueueScreen "       TOP      ","     PIRATES    ",0,4,0,2000,False,""
	DisplayQueueScreen "    1)  "&gsHighScoreName(1)&"    ","   "&FormatScore(gsHighScore(1))&" ",0,0,0,1500,False,""
	DisplayQueueScreen "    2)  "&gsHighScoreName(2)&"    ","   "&FormatScore(gsHighScore(2))&" ",0,0,0,1500,False,""
	DisplayQueueScreen "    3)  "&gsHighScoreName(3)&"    ","   "&FormatScore(gsHighScore(3))&" ",0,0,0,1500,False,""
	DisplayQueueScreen "    4)  "&gsHighScoreName(4)&"    ","   "&FormatScore(gsHighScore(4))&" ",0,0,0,1500,False,""
	DisplayQueueScreen " MASTER MARINER "," "&gsHighScoreName(0)&"  "&FormatScore(gsHighScore(0)),8,8,0,2000,False,""
	DisplayQueueScreen " LOOP CHAMP " &gsLoopChampName," WITH "&FormatScore1K(gsLoopChamp,"","")& " LOOPS",3,3,0,2000,False,""
	AttractLights()
End Sub

Dim AttractLightsStep:AttractLightsStep = 1
Dim tester:tester = 0
Sub AttractLights()
	TiLights.Enabled = 0
	Select Case AttractLightsStep
	Case 1
		Attract1.UpdateInterval=10
		Attract1.Play SeqCircleOutOn ,20,3,50
		Attract1.Play SeqClockRightOn ,90,3,50
		Attract1.Play SeqMiddleOutVertOn ,20,3,50
		AttractLightsStep = 2
	Case 2
		SeqModes.UpdateInterval=10
		SeqMisc.UpdateInterval=10
		SeqInsult.UpdateInterval=10
		SeqShots.UpdateInterval=10
		SeqLechuck.UpdateInterval=10
		SeqModes.Play SeqRandom, 1, , 20000
		SeqMisc.Play SeqScrewRightOn, 90, 50, 1
		SeqInsult.Play SeqBlinking, 10, 1000, 500
		For X = 1 to 10
			SeqShots.Play SeqRightOn, 60, 2, 1
			SeqShots.Play SeqLeftOn, 60, 2, 1
			SeqLeChuck.Play SeqUpOn, 10, 1, 1
			SeqLeChuck.Play SeqDownOff, 10, 1, 1
		Next
		AttractLightsStep = 1
	End Select
End Sub

Sub SeqModes_PlayDone()
	StopSeqs()
	If InAttract = 1 Then
		AttractLights()
	End if
	TiLights.Enabled = 1
End Sub

Sub Attract1_PlayDone()
	If InAttract = 1 Then
		AttractLights()
	End if
	TiLights.Enabled = 1
End Sub

Sub StopSeqs()
	SeqInsult.StopPlay
	SeqMisc.StopPlay
	SeqModes.StopPlay
	SeqShots.StopPlay
	SeqLeChuck.StopPlay
	Attract1.StopPlay
	TiLights.Enabled = 1
End Sub

' ************************* Launch and Drain Routines **********************
Sub Bang() 'launch main
	FlashSword
	PlaySound "fire"
	PlaySound "balloutr"
	BallSave = 1
	BallSaveTimer.Enabled=False
	BallSaveTimer.Enabled=True
	EndMusic()
	Song="EFMI_MonkeyTheme.mp3"
	PlayMusic Song
	DisplayFlushQueue
	DisplayQueueScreen "      BANG      ","                ",8,0,0,250,False,""
	DisplayQueueScreen "      BANG      ","               O",8,0,0,10,False,""
	DisplayQueueScreen "      BANG      ","              O ",8,0,0,10,False,""
	DisplayQueueScreen "      BANG      ","             O  ",8,0,0,10,False,""
	DisplayQueueScreen "      BANG      ","            O   ",8,0,0,10,False,""
	DisplayQueueScreen "      BANG      ","           O    ",8,0,0,10,False,""
	DisplayQueueScreen "      BANG      ","          O     ",8,0,0,10,False,""
	DisplayQueueScreen "      BANG      ","         O      ",8,0,0,10,False,""
	DisplayQueueScreen "      BANG      ","        O       ",8,0,0,10,False,""
	DisplayQueueScreen "      BANG      ","       O        ",8,0,0,10,False,""
	DisplayQueueScreen "      BANG      ","      O         ",8,0,0,10,False,""
	DisplayQueueScreen "      BANG      ","     O          ",8,0,0,10,False,""
	DisplayQueueScreen "      BANG      ","    O           ",8,0,0,10,False,""
	DisplayQueueScreen "      BANG      ","   O            ",8,0,0,10,False,""
	DisplayQueueScreen "      BANG      ","  O             ",8,0,0,10,False,""
	DisplayQueueScreen "      BANG      "," O              ",8,0,0,10,False,""
	DisplayQueueScreen "      BANG      ","O               ",8,0,0,10,False,""
	DisplayQueueScreen "      BANG      ","                ",8,0,0,10,False,""
	LightArray(23, 1) = 3
	TiBang.Enabled = 1
	BallsInPlay=BallsInPlay+1			'EP- Probably should put in some checking if the ball doesn't actually make it to the playfield
	ComboMult=0
	BallStop=0
End Sub

Sub TiBang_Timer()
	Me.Enabled=False
	LightArray(23, 1) = 0
	LightArray(55, 1) = 0
End Sub

'************************
'*	Shoopity's Trough stuff
'************************

Sub KickOut_Hit()
	Set BallMover = ActiveBall
	Me.UserValue = 1
	If KiMainTrough3.UserValue = 0 Then
		KiMainTrough2.Kick 50, 5, 0
		KiMainTrough2.UserValue = 0
	End If
End Sub

Sub Wall4_Hit()
	If KickOut.UserValue = 0 Then
		KiMainTrough3.Kick 50, 5, 0
		KiMainTrough3.UserValue = 0
	End If
End Sub

Sub ServeBall()
	KickOut.Kick 80, 5, 0
	Set BallMover = Nothing
	KickOut.UserValue = 0
End Sub

Sub MoveBall(ball, dest, source, S1, S2, S3)						'The precious ball mover routine that moves the Ball, to the Dest, and making sure the Source is what kicks it
	ball.X = dest.X
	ball.Y = dest.Y
	source.kick S1, S2, S3
End Sub

Sub TroughTimer_Timer() 'delays balls from entering drain until timer has run out
' TroughTimer is for "special" cases (flag BallStop=1)
' Special cases are: 	- lost of ball with BallSave=1 in Voodoo and MultiBall
' 						- SuperJackpot, MouthRoutine and end in MultiBall
' Timer is kept active and only stopped when we are in ModeTotalTimer,
' KickOutTimer or Newball (ResetAll). Then, BallStop is turned to 0...
' So we can do what to do before next ball go in drain...
	If BallStop=1 Then Exit Sub								' have to wait more
	TroughTimer.Enabled=False
	BallStopper.IsDropped=True	' next ball can go
End Sub

Dim BallMover, BallMover2, BallMover3, BallMover4
Sub Drain_Hit()
	If ActiveBall.ID = 21 Then Drain.DestroyBall:Exit Sub
	'let's establish the things that are always going to happen
	BallsinPlay = BallsinPlay - 1 'this really SHOULD go here!
	PlaySound "kickgrog"
	GoDrain()
	Playsound "drain"
	Set BallMover3 = ActiveBall							'Set the object so that we can...
	If ActiveBall.ID = 666 Then
		MoveBall BallMover3, mAGNET2kicker, me, 0, 0, 0			'If this is the voodoo ball, move it back up there
		VooDooKeeper.IsDropped = 0
	Else
		MoveBall BallMover3, KiMaintrough1, me, 58, 8, 0		'Move the ball, to the main trough, coming from the drain
	End If
	Set BallMover3 = Nothing
End Sub

Sub KiMainTrough1_Hit()									'When the bottom most kicker gets hit...
	Me.UserValue = 1
	MoveIt()
End Sub

Sub KiMainTrough2_Hit()									'For the rest of the kickers, when they get hit, we'll...
	Me.UserValue = 1
	MoveIt()
End Sub

Sub KiMainTrough3_Hit()
	Me.UserValue = 1
	MoveIt()
End Sub

Sub KickOut_Timer()										'This gets turned on at various times such as a ball save, or for multiball
	Me.TimerEnabled=False
	ComboMultiTimer.Enabled=False						'Disable Combo Timer
	ComboMult=0
	LightArray(23, 1) = 3								'Flash the cannon
	TiBang.Enabled = 1									'And turn it off
	If Status<>"GoEscape" Then NudgeOkay=1
	KickOut.Timerinterval=1500							'Reset this timer?
	AutoPlunger.AutoFire
	BallsInPlay=BallsInPlay+1
	FlashSword
	Select Case Status
	Case "Normal"
		DisplayScoreQueue 1,2
		PlaySound "fire"
		If BallSaved=1 Then
			BallSaved=0
			LightArray(22, 1) = 2
		Else
			BallSave=0
			LightArray(22, 1) = 0
		End If
	Case "Multiball"
		PlaySound "explode1"
		PlaySound "eatdeath"
		HeadFlash
	Case "GoEscape"
		PlaySound "explode1"
		If EscapeActive=0 Then Status="Normal"
	Case Else
		PlaySound "fire"
	End Select
	BallStop=0
End Sub

Sub MoveIt()
	If KiMainTrough2.UserValue = 0 Then
		KiMainTrough1.Kick 50, 5, 0
		KiMainTrough1.UserValue = 0
	End If
	If KiMainTrough3.UserValue = 0 Then
		KiMainTrough2.Kick 50, 5, 0
		KiMainTrough2.UserValue = 0
	End If
	If KickOut.UserValue = 0 Then
		KiMainTrough3.Kick 50, 5, 0
		KiMainTrough3.UserValue = 0
	End If
End Sub

'************************ End Trough Stuff

Sub GoDrain
	SaveLights()
	If Tilt=1 Then
		Bonus=1000: ExtraBalls=0: BonusHeld=0
	End If
	Select Case Status
	Case "Normal"
		If BallSave=0 Then
			If BallsInPlay<1 Then		' we can have 2 balls in play...
				FinishDrain()
			End If
		Else
			CenterArrowsFlash()
			ComboActive=0
			DisplayFlushQueue()
			DisplayQueueScreen "    GUYBRUSH    ","    RETURNS     ",8,8,0,1500,False,"imback"
			ServeBall()
			LightArray(22, 1) = 0
			KickOut.TimerEnabled=True
		End If
	Case "VoodooMagic"
		If BallSave=0 Then
			HurryUpWaitTimer.Enabled=False
			HurryUpTimer.Enabled=False
			HurryUpEndTimer.Enabled=False
			MagnetsOffMain()
			If BallsInPlay=1 Then
				If Tilt=1 Then Exit Sub
				DisplayFlushQueue()
				DisplayQueueScreen "  VOODOO  BALL  ","    VANISHES    ",8,8,0,1500,False,""
			End If
			GoModeTotal()
			LightArray(22, 1) = 0
		End If
		If BallSave=1 Then
			BallStop=1
			ServeBall()
			DisplayFlushQueue()
			DisplayQueueScreen "      DONT      ","      MOVE      ",3,3,0,1500,False,""
			LightArray(22, 1) = 0
			KickOut.TimerEnabled=True
		End If
	Case "Spitting","HighDive","SwordMaster","DaintyLady","MonkeyCombat","GraveDigger","TreasureHunt"
		If BallSave=1 Then
			If Status="SwordMaster" Or Status="TreasureHunt" Then
				Set BallSM = BallMover
			End If
			ServeBall()
			DisplayFlushQueue()
			DisplayQueueScreen "      DONT      ","      MOVE      ",3,3,0,1500,False,""
			LightArray(22, 1) = 0
			KickOut.TimerEnabled=True
		Else
			GoModeTotal()
		End If
	Case "Multiball"
		If BallSave=1 Or LightArray(22, 1) = 2 Then
			BallStop=1
			ServeBall()
			KickOut.TimerEnabled=True
			If MultiTimer.Enabled=False Then
				BallSaveTimer.Enabled=False
				BallSave = 0
				LightArray(22, 1) = 0
			End If
		Else
			If BallsinPlay=1 Then 'back to normal
				MultiballEnd()
			Else 'display balls in play
				DisplayFlushQueue()
				DisplayQueueScreen "        "&BallsinPlay&"     ","  BALLS IN PLAY ",3,0,0,1000,True,""
				DisplayScoreQueue 0,0
			End If
		End If
	Case "GoEscape"
		If BallSave=1 Then
			ServeBall()
			KickOut.TimerEnabled=False
			KickOut.TimerEnabled=True
		Else 'drain the last ball and stop drain timer from kicking ball - do this in escape mode total countup
			If BallsinPlay=0 Then
				PlaySound "balloutr"
				TotalEscapeMode()
			End If
		End If
	End Select
End Sub

Sub Drain_Timer()
	Drain.TimerEnabled=False 'end of drain
	If BallsRemaining=0 Then
		EndOfGame
	Else
		NudgeOkay=1
		NewBall()
	End If
End Sub

Sub FinishDrain()
	'added so this usual end response can
	'be jumped to after other mode timers etc have expired
	MoveMouthGuard(-1)
	EndMusic()
	Attract1.Play SeqCircleOutOn ,100,,10000
	PlaySound "drain"
	LeftFlipper.RotateToStart
	RightFlipper.RotateToStart
	Drain.TimerEnabled=True
	DisplayFlushQueue
	DisplayQueueScreen "  LOST  AT  SEA ","/\_/\_/\_/\_/\_/",0,0,0,80,False,"splash44"
	DisplayQueueScreen "  LOST  AT  SEA ","_/\_/\_/\_/\_/\_",0,0,0,80,False,""
	DisplayQueueScreen "  LOST  AT  SEA ","\_/\_/\_/\_/\_/\",0,0,0,80,False,""
	DisplayQueueScreen "  LOST  AT  SEA ","/\_/\_/\_/\_/\_/",0,0,0,80,False,""
	DisplayQueueScreen "  LOST  AT  SEA ","_/\_/\_/\_/\_/\_",0,0,0,80,False,""
	DisplayQueueScreen "  LOST  AT  SEA ","\_/\_/\_/\_/\_/\",0,0,0,80,False,""
	DisplayQueueScreen "  LOST  AT  SEA ","/\_/\_/\_/\_/\_/",0,0,0,80,False,""
	DisplayQueueScreen "  LOST  AT  SEA ","_/\_/\_/\_/\_/\_",0,0,0,80,False,""
	DisplayQueueScreen "  LOST  AT  SEA ","\_/\_/\_/\_/\_/\",0,0,0,80,False,""
	DisplayQueueScreen "  LOST  AT  SEA ","/\_/\_/\_/\_/\_/",0,0,0,80,False,""
	DisplayQueueScreen "  LOST  AT  SEA ","_/\_/\_/\_/\_/\_",0,0,0,80,False,""
	DisplayQueueScreen "  LOST  AT  SEA ","\_/\_/\_/\_/\_/\",0,0,0,80,False,""
	DisplayQueueScreen "                ","                ",6,6,0,500,False,""
	DisplayQueueScreen "     BONUS      ","      X "&BonusMultiplier&"      ",0,0,0,1250,False,"monkeyend"
	If BonusHeld = 1 Then BonusHeld=Bonus
	Bonus=Bonus*BonusMultiplier
	DisplayQueueScreen " "&FormatScore(Bonus)&" ","                ",0,0,0,500,False,""
	DisplayQueueScreen "  COMBAT LEVEL  ","       "&CombatLevel&"        ",0,0,0,500,False,"monkeyend"
	Bonus=Bonus+(CombatLevel*500000)
	DisplayQueueScreen "       "&ModesStarted&"   ","  MODES STARTED ",0,0,0,500,False,"monkeyend"
	Bonus=Bonus+(ModesStarted*1000000)
	AddScore(Bonus)
	DisplayQueueScreen " TOTAL BONUS =  ","  "&FormatScore(Bonus)&"  ",0,0,0,2500,true,"monkeyend"
	If BonusHeld = 0 Then
		Bonus=1000
	Else
		DisplayQueueScreen "   BONUS HELD   ","  "&FormatScore(BonusHeld)&"  ",3,3,0,1000,true,"monkeyend"
		Bonus=BonusHeld: BonusHeld=0
	End If
	If ExtraBalls=0 Then
		Ball=Ball+1
		BallsRemaining=BallsRemaining-1 'total balls left
	End If
	LightArray(21, 1) = 0
	PFMultiplier=1
End Sub

' ********************************* SCORING ROUTINES ***********************

Sub AddScore(Points)
	Score=Score+(Points*PFMultiplier)
	Jackpot=Jackpot+250
	If ReplayGiven=0 And Score>gsReplayValue Then
		ReplayGiven=1: Credits=Credits+1
		DisplayFlushQueue
		DisplayQueueScreen "     REPLAY     ","     AWARDED    ",8,8,0,1000,False,"knocker"
		DisplayQueueScreen "     REPLAY     ","     AWARDED    ",3,3,0,1000,False,"cheer1"
	End If
	DisplayScore
End Sub

Sub AddBonus(Bpoints)
	Bonus=Bonus+BPoints
End Sub
'********************************************************************************

' ************************* Start Game *************************************
' Assign values to all variables except credits, individual mode settings,
' scoring and table initialization.

Sub StartGame()
	mAGNET2kicker.kick 0, 0, 0
	If BallsPerGame = 3 Then
		gsReplayValue=100000000
	Else
		gsReplayValue=200000000
	End If
	GamesPlayed=GamesPlayed+1
	If GamesPlayed=2 Then PlaySound "helloagain"
	SpittingDistance(1)=40: SpittingDistance(2)=60: SpittingDistance(3)=80: SpittingDistance(4)=100
	Select Case Play_Mode
	Case "Easy"
		VoodooItemTodo=3
		DiveToDo=4
		SwordToDo=2
		MadCombatToDo=2
		DaintyLadyScores(1)=3
		DaintyLadyScores(2)=3
		DaintyLadyScores(3)=4
		GraveDiggerScores(1)=2
		GraveDiggerScores(2)=2
		GraveDiggerScores(3)=3
		GraveDiggerScores(4)=3
		TreasureHuntScores(1)=2
		TreasureHuntScores(2)=2
		TreasureHuntScores(3)=3
		TreasureHuntScores(4)=3
		CoeffTimer=1.3
		ModeTotalCoeff=5000000
		CoeffGoEscape=750000
		SpitCoeff=25000
		SpitAdjust=50
		ComboTimer.Interval=6000
		ComboMultiTimer.Interval=5000
		MaxHurryUp=999
	Case "Medium"
		VoodooItemTodo=4: DiveToDo=6: SwordToDo=3: MadCombatToDo=3
		DaintyLadyScores(1)=4: DaintyLadyScores(2)=5: DaintyLadyScores(3)=6
		GraveDiggerScores(1)=3: GraveDiggerScores(2)=3: GraveDiggerScores(3)=4: GraveDiggerScores(4)=5
		TreasureHuntScores(1)=3: TreasureHuntScores(2)=3: TreasureHuntScores(3)=4: TreasureHuntScores(4)=5
		CoeffTimer=1: ModeTotalCoeff=6000000: CoeffGoEscape=1000000
		SpitCoeff=35000: SpitAdjust=25
		ComboTimer.Interval=5000: ComboMultiTimer.Interval=4250
		MaxHurryUp=850
	Case "Hard"
		VoodooItemTodo=5: DiveToDo=6: SwordToDo=4: MadCombatToDo=3
		DaintyLadyScores(1)=6: DaintyLadyScores(2)=6: DaintyLadyScores(3)=8
		GraveDiggerScores(1)=4: GraveDiggerScores(2)=5: GraveDiggerScores(3)=5: GraveDiggerScores(4)=6
		TreasureHuntScores(1)=4: TreasureHuntScores(2)=5: TreasureHuntScores(3)=5: TreasureHuntScores(4)=6
		CoeffTimer=0.75: ModeTotalCoeff=7500000: CoeffGoEscape=1500000
		SpitCoeff=50000: SpitAdjust=0
		ComboTimer.Interval=4000: ComboMultiTimer.Interval=3500
		MaxHurryUp=700
	End Select
	Kicker3Hits=0
	Kicker3Delay=2000
	Super=0
	ResetItemsLights()	' EscapeItems lights
	ResetAll() 			' lights
	MagnetsOffMain
	EB=0
	MoveMouthGuard(-1)
	Bananas=0
	BananaPicker=0
	ModeCount=0
	For x=1 To 8
		ModesGet(x)=0
	Next
	ModesStarted=0
	ModeTotal=0
	ModeTimeLeft=0
	TextBox1.Text= ""
	Barrels=0
	BarrelScore=2500
	HeadHits=0
	MouthHits=0
	Combos=0
	GrogHits=0
	GrogTotal=0
	GameStarted=1
	EscapeItems=0
	EscapeActive=0
	Status="Normal"
	HurryUpBonus=0
	PFMultiplier=1
	ExtraBalls=0
	Ball=1
'	BallsInPlay=1
	BallsRemaining=BallsPerGame
	Score=0
	ReplayGiven=0
	GoComplete=0
'	BallSave=1
	LockedBalls=0
	MaxLoopsThisGame=0
	LBounce=0
	MBounce=0
	RBounce=0
	Points=0
	BPoints=0
	Bonus=1000
	BonusMultiplier=1
	ComboMult=0
	Multiball=0
	SwingForce=0
	Bob=1
	Tilt=0
	SwingTotal=0
	NextSpitKey=1
	II="*"
	NN="*"
	SS="*"
	UU="*"
	LL="*"
	TT="*"
	CombatLevel=0
	CombatAward="10000000"
	CombatCount=0
	Combat1Count=0
	Combat2Count=0
	Combat3Count=0
	VoodooCount=0
	VooDooPopUp.IsDropped=True
	VooDooItem=0
	Plungeokay=1
	Flipokay=1
	InAttract = 0
	StopSeqs()
	DisplayFlushQueue
	DisplayQueueScreen "     LAUNCH     ","      BALL      ",3,3,0,1000,False,""
	ServeBall()
	LightArray(55, 1) = 3
	PlaySound "solon"
	Song="EFMI_Intro.mp3"
	PlayMusic Song
	LightArray(22, 1) = 2
	CenterArrowsFlash
	SpitDistance=0
	ScummLoops=0
	NextScummAward=1
	ScummOff()
	ScummTarget.IsDropped=True
	CancelModeLights()
	LightArray(38, 1) = 0
	'BallStopper.IsDropped=True
	'MouthIn.Enabled=False 		 'disable mouth kicker
	'MouthMiss.Enabled=False		 'and the debug one
	MultiballEnded=0
	SwordTarget=0
	SwordNumber=0
	ShipNumber=1
	ShipTarget=0
	MadSetNumber=0 				'mad monkey combat variables
	MadTargetNumber=0
	MadNumber=0
	MadTarget(1)=0
	MadTarget(2)=0
	MadTarget(3)=0
	CorrectMadHits=0
	CorrectMadSet=0
	MadAllowed=0
	WhatMadTarget=0
	EscapeItem1=0
	EscapeItem2=0
	EscapeItem3=0
	EscapeItem4=0
	EscapeItem5=0
	EscapeItem6=0
	EscapeItem7=0
	EscapeItem8=0
	LightArray(43, 1) = 0
	LightArray(44, 1) = 0
	LightArray(45, 1) = 0
	ResetModeLights()
	AdvanceModeCount()
	MonkeyBeat=0
'	KickoutReminderTimer.Enabled=True
	InsultOff()
	Lightning()
End Sub

Sub Light24_Timer()
	TempG = TempG - 2*ColorDir
	If TempG >= 254 Then ColorDir = ColorDir*-1
	If TempG <= 2 Then ColorDir = ColorDir*-1
	Light24.Color = RGB(TempR, TempG, 0)
	Light24.ColorFull = RGB(TempR, TempG, 0)
End Sub

Dim LightArray(64, 2), LightTicker, BlinkSpeed
Const MaxLights = 62
For X = 0 to MaxLights
	Set LightArray(x, 0) = Inserts(x)
Next
LightTicker = 0
BlinkSpeed = 10
Sub TiLights_Timer()
	'0 = off
	'1 = on
	'2 = blink
	'3 = strobe
	LightTicker = LightTicker + 1
	For X = 0 to MaxLights
		Select Case LightArray(x, 1)
		Case 0
			LightArray(x, 0).State = 0					'If the lights are set to 0, make sure they're off
		Case 1
			LightArray(x, 0).State = 1					'If the lights are set to 1, make sure they're on
		Case 2
			If LightTicker MOD BlinkSpeed = 0 Then LightArray(x, 0).State = 0			'Flip their state
			If LightTicker MOD BlinkSpeed*2 = 0 Then LightArray(x, 0).State = 1			'Flip their state
		Case 3											'If the light is set to strobe...
			If LightTicker MOD 2 = 0 Then
				LightArray(x, 0).State = 0				'Turn off every other light tick...
			Else
				LightArray(x, 0).State = 1				'Turn On every other light tick
			End If
		End Select
	Next
	If LightTicker >= 100 Then LightTicker = 0			'Reset
End Sub

Sub ResetAll()
	StopSeqs()
	For X = 0 to MaxLights
		LightArray(x, 1) = 0
	Next
	ResetMonkeys
	'Reset all timer interval wich can change
	KickOut.TimerInterval=1500
	EKick.TimerInterval=5000
	AKick.TimerInterval=5000
'	OKick.TimerInterval=5000
	Kicker3.TimerInterval=2000
	BallSaveTimer.Interval=15000
	BallSave=0
	TiScummKicker.Interval=5000
	NewMadTargetTimer.Interval=9500
	TiKicker6.Interval=3000
	JackPotTimer.Interval=4000
	ModeTotalTimer.Interval=4000
	BallStop=0
	'variables
	SwingForce=0
	Bob=1
	SwingTotal=0
End Sub

' *************************************** NEW BALL *****************************************************

Sub NewBall()
	ResetAll														'EP- Not going to reset on every ball, that's nutty
	LoadLights()
	For X = 14 to 20												'EP- I think the Multiplier lights should only be turned on when they've been gotten
		LightArray(X, 1) = 0
	Next
	If BonusMultiplier > 1 Then										'EP- apparently the bonus mutliplier continues throughout the game?
		LightArray(((BonusMutlipier/2)+13), 1).State = 1
	End If
	Kicker3Hits=0
	If VoodooCount=3 Then
		Kicker6.Enabled=True
		LightArray(46, 1) = 2
	End If
	ActiveMonkeyBeat
	LightArray(55, 1) = 3
	MoveMouthGuard(-1)
	GrogKicker.Enabled=False
	DisplayScoreQueue 2,1
	GrogLight(-1)
	GiGrogTimer.Enabled = 0
	GrogHits=0
	ComboMult=0
	PlaySound "solon"
	CenterArrowsFlash
	ServeBall()
	DisplayFlushQueue
	If ModesStarted=8 Then DisplayModesCompleted
	If ReplayGiven=0 And BallsRemaining=1 Then
		DisplayQueueScreen "   REPLAY  AT   ","   " &FormatScore(gsReplayValue) & "   ",3,3,0,500,False,""
		DisplayQueueScreen "   REPLAY  AT   ","   " &FormatScore(gsReplayValue) & "   ",3,8,0,500,False,""
	End If
	If ExtraBalls > 0 Then
		DisplayQueueScreen "     EXTRA      ","      BALL      ",8,8,0,500,False,""
		DisplayQueueScreen "     SHOOT      ","     AGAIN      ",3,3,0,500,False,""
		ExtraBalls=ExtraBalls-1
	Else
		DisplayQueueScreen "     LAUNCH     ","      BALL      ",3,3,0,1000,False,""
	End If
	If EB=1 Then EBLight.State=LightStateBlinking
	If ExtraBalls > 0 Then
		EBLitLight.State=LightStateOn
		EBScoredThisBall=0
	End If
	Plungeokay=1
	Flipokay=1
	EndMusic()
	Song="EFMI_Intro.mp3"
	PlayMusic Song
'	BallSave=1
	LightArray(22, 1) = 2
	Tilt=0
	BallSaved=0
	Status="Normal"
	If (II="I") And (NN="N") And (SS="S") And (UU="U") And (LL="L") And (TT="T") Then 'light modes if ready
		ScummOn()
	End If
	If BananaPicker=1 Then BananaLight.State=LightStateOn
	KickoutReminderTimer.Enabled=True
End Sub

Sub KickoutReminderTimer_Timer()
	KickoutReminderTimer.Enabled=False
	DisplayQueueScreen "     PRESS      ","     ENTER      ",3,3,0,1000,true,"init5"
	DisplayQueueScreen "     LAUNCH     ","      BALL      ",3,3,0,1000,False,""
End Sub

Sub GrogLight(state)
	Select Case state
	Case -1
		GIGrog.State = 0
		PrGrog.Image = "Grog off"
	Case 1
		GIGrog.State = 1
		PrGrog.Image = "Grog on"
	End Select
End Sub

Sub GiGrogTimer_Timer()
	Select Case GiGrog.State
	Case 0
		GrogLight(1)
	Case 1
		GrogLight(-1)
	End Select
End Sub

Sub ResetHead()
	MoveHead(180)
	MoveMouth(0)
End Sub

' ************************************** END OF GAME ***************************************************

Sub EndOfGame()
	GameStarted=0
	Plungeokay=0
	DisplayFlushQueue
	DisplayQueueScreen "      GAME      ","      OVER      ",1,2,0,1500,False,""
	ResetItemsLights
	ResetAll
	Flipokay=0
	ResetMonkeys
	BlinkAllLights
	Kicker6.Enabled=False
	VooDooMag.MagnetOn = 0
	LightArray(46, 1) = 0						'Mystery Light off?
	If HeadHits=4 Then
		MouthClose
	Else
		ResetHead
	End If

	If Score<10000000 Then PlaySound "whatamisupposed"
	MaxLoopsThisGame=ScummLoops
	If (Score > gsHighScore(4)) Or (MaxLoopsThisGame > gsLoopChamp) Then
		HighScoreEntryInit()
	Else
		Song="EFMI_Spooky.mp3"
		PlayMusic Song
		MusicEndTimer.Enabled=True
		GameStarted=0
		LeftFlipper.RotateToStart
		RightFlipper.RotateToStart
		gsLastScore=Score
		gsGamesPlayed=gsGamesPlayed+1
		If ReplayGiven=1 Then gsReplayValue= gsReplayValue+1000000
		If ReplayGiven=0 And gsReplayValue>2000000 Then gsReplayValue= gsReplayValue-5000000
		DisplayQueueScreen "   "&FormatScore(gsLastScore)&"   ", "      "&hsEnteredName&"       ", 0, 3, 0,2000,False,""
		InAttract = 1
		AttractMode()
		If Credits>0 Or FreePlay=1 And GameStarted=0 Then
			'EP- Blink Start Light
			TextBox1.Text= "Press 1 To Start"
			DisplayQueueScreen "   CREDITS "&Credits&"    ","  PRESS  START  ",3,0,0,2000,False,""
		End If
		If Credits=0 And freeplay=0 And GameStarted=0 Then
			'EP- Turn off start light
			TextBox1.Text= "Press 5 For Credit"
			DisplayQueueScreen "     INSERT     ","      COINS     ",3,3,0,1000,False,""
		End If
	End If
End Sub

Dim HeadDest, HeadDir, HeadSpeed, MouthDest, MoughtDir, Mouthspeed
HeadDir = 1
HeadSpeed = 0.5
MouthDir = 1
MouthSpeed = 0.3
'Set the destination ranging between 180 and -180 and turn on the motor (i.e. the timer); also set the direction the head should turned
Sub MoveHead(dest)
	TiHeadMover.Enabled = 1
	HeadDest = dest
	If HeadDest > PrMonkeyHead.ObjRotZ Then HeadDir = 1 Else HeadDir = -1
End Sub

'Turns the head in the previously set direction, once the destination is reached (or passed), the motor (timer) is turned off
Sub TiHeadMover_Timer()
	Dim DestReached
	PrMonkeyHead.ObjRotZ = PrMonkeyHead.ObjRotZ + (HeadSpeed*HeadDir)
	If (HeadDir = 1 AND HeadDest <= PrMonkeyHead.ObjRotZ) OR (HeadDir = -1 AND HeadDest >= PrMonkeyHead.ObjRotZ) Then
		DestReached = 1
	End If
	If DestReached = 1 Then me.enabled = 0
	PrMonkeyMouth.ObjRotZ = PrMonkeyHead.ObjRotZ
	PrMonkeyMouth.Y = dSin(PrMonkeyHead.ObjRotZ+90)*75 + PrMonkeyHead.Y
	PrMonkeyMouth.X = dCos(PrMonkeyHead.ObjRotZ+90)*75 + PrMonkeyHead.X
End Sub

Sub MoveMouth(OoC)
	TiMouthMover.Enabled = 1
	Select Case OoC
	Case 1
		MouthDir = 1
	Case 0
		MouthDir = -1
	End Select
End Sub

Sub TiMouthMover_Timer()
	Dim DestReached
	PrMonkeyMouth.RotX = PrMonkeyMouth.RotX + -(MouthDir*MouthSpeed)
	If (PrMonkeyMouth.RotX <= -40) OR (PrMonkeyMouth.RotX >= 0) Then
		DestReached = 1
	End If
	If DestReached = 1 Then
'		Me.enabled = 0
		MouthDir = MouthDir * -1
	End If
End Sub

Dim MouthGuardDir, MouthGuardSpeed
MouthGuardSpeed = 1
Sub MoveMouthGuard(dir)
	TiMouthGuardMover.Enabled = 1
	MouthGuardDir = dir
End Sub

Sub TiMouthGuardMover_Timer()
	PrMouthguard.Z = PrMouthguard.Z + (MouthguardSpeed*MouthguardDir)
	If PrMouthguard.Z <= -50 Then
		Me.Enabled = 0
		MouthGuard.IsDropped = 1
	End If
	If PrMouthguard.Z > 50 Then
		Me.Enabled = 0
		MouthGuard.IsDropped = 0
	End If
End Sub

Sub ResetMonkeys()
	'Hopefully I'll have Primitive monkeys so here I'll have stuff like:
	'PrMonkey1.Z = ResetPos
	Combat1Count=0: Combat2Count=0: Combat3Count=0
End Sub

Sub ResetHiScores()
	gsHighScoreName(0)="IAN": gsHighScore(0)=120000000
	gsHighScoreName(1)="MON": gsHighScore(1)=80000000
	gsHighScoreName(2)="KEY": gsHighScore(2)=60000000
	gsHighScoreName(3)="ISL": gsHighScore(3)=40000000
	gsHighScoreName(4)="AND": gsHighScore(4)=20000000
	gsLoopChampName="IAN": gsLoopChamp=1
End Sub

'*************************** MODES LIT **********************************************

Sub ModesLit()
	DisplayFlushQueue
	EndCurrentInsult
	DisplayQueueScreen "  "&II&" "&NN&" "&SS&" "&UU&" "&LL&" "&TT&"   ","   MODES  LIT   ",3,3,0,1000,False,""
	DisplayQueueScreen "  SHOOT FOR THE ","   SCUMM  BAR   ",8,8,0,1000,False,""
	InsultBlinking
	If LightArray(62, 1) = 0 Then
		PlaySound "thatsnotinsult"
		PlaySound "flapopen"
	End If
	LightArray(62, 1) = 1
	SetPulse 62, 1
	PlaySound "solon"
	DisplayScoreQueue 1,2
	ScummOn
End Sub

Sub ScummOff()
	PrScumm.Image = "Scumm Bar Off"
	GiScumm.State = 0
	ScummMag.MagnetOn = 0
	ScummTrapper.IsDropped = 1
End Sub

Sub ScummOn()
	PrScumm.Image = "Scumm Bar On"
	GiScumm.State = 1
	ScummMag.MagnetOn = 1
End Sub

Sub InsultOff()
	For X = 56 to 61
		LightArray(x, 1) = 0
	Next
	'reset insult
	II="*": NN="*": SS="*": UU="*": LL="*": TT="*"
End Sub

Sub InsultBlinking()
	For X = 56 to 61
		LightArray(x, 1) = 2
	Next
End Sub

Sub InsultOn()
	For X = 56 to 61
		LightArray(x, 1) = 1
	Next
End Sub

Sub ResetLeChuck()
	Light100.State=LightStateOff: Light40.State=LightStateOff: Light10.State=LightStateOff: Light24.State=LightStateOff
	Chuck1Light.State=LightStateOff: Chuck2Light.State=LightStateOff: Chuck3Light.State=LightStateOff: ChuckBeatLight.State=LightStateOff
End Sub

Sub ResetItemsLights()
	LightArray(4, 1) = 0			'Totem
	LightArray(7, 1) = 0			'Map
	LightArray(26, 1) = 0			'Gold Bars
	LightArray(27, 1) = 0			'Seagul?
	LightArray(54, 1) = 0			'Documents
	LightArray(13, 1) = 0			'Monkey?
End Sub

Sub BlinkAllLights()
	For x = 0 to MaxLights
		LightArray(x, 1) = 2
	Next
	ResetModeLights()				' :)
End Sub

Sub CenterArrowsFlash()
	LightArray(33, 1) = 2
	LightArray(32, 1) = 2
	LightArray(42, 1) = 2
	LightArray(31, 1) = 2
	LightArray(30, 1) = 2
	LightArray(29, 1) = 2
	LightArray(28, 1) = 2
	LightArray(41, 1) = 2
End Sub

Sub CenterArrowsOff()
	LightArray(33, 1) = 0
	LightArray(32, 1) = 0
	LightArray(42, 1) = 0
	LightArray(31, 1) = 0
	LightArray(30, 1) = 0
	LightArray(29, 1) = 0
	LightArray(28, 1) = 0
	LightArray(41, 1) = 0
End Sub

Sub CenterArrowsOn()
	LightArray(33, 1) = 1
	LightArray(32, 1) = 1
	LightArray(42, 1) = 1
	LightArray(31, 1) = 1
	LightArray(30, 1) = 1
	LightArray(29, 1) = 1
	LightArray(28, 1) = 1
	LightArray(41, 1) = 1
End Sub

' ****************************** Lighting Effects **************************
' Shoopity - I'm making my own lightning effect
Dim LightMemory(64)

Sub SaveLights()
	For X = 0 to MaxLights
		LightMemory(x) = LightArray(x, 1)
	Next
End Sub

Sub LoadLights()
	For X = 0 to MaxLights
		LightArray(x, 1) = LightMemory(x)
	Next
End Sub

Sub Lightning()
	SaveLights()
	For X = 0 to MaxLights
		LightArray(x, 1) = 3
	Next
	Fader()
End Sub

Sub Fader()
	For X = 0 to MaxLights
		LightArray(x, 0).Intensity = 30
	Next
	TiFader.Enabled = 1
End Sub

Dim FadeAmount:FadeAmount = 0.25
Sub TiFader_Timer()
	For X = 0 to MaxLights
		LightArray(x, 0).Intensity = LightArray(x, 0).Intensity - FadeAmount
	Next
	If LightArray(0, 0).Intensity = 0 Then
		Me.enabled = 0
		For X = 0 to MaxLights
			LightArray(x, 1) = LightMemory(x)
			LightArray(x, 0).Intensity = 15
		Next
	End If
End Sub

Dim IsPulsing(64), PulseDir
PulseDir = 1
For X = 0 to 63
	IsPulsing(x) = 0
Next
Sub SetPulse(which, OnOrOff)
	IsPulsing(which) = OnOrOff
	If OnOrOff <> 1 Then LightArray(which, 0).Intensity = IntensityMem(which)
End Sub

Dim Intensitymem()
ReDim IntensityMem(MaxLights)
Sub StoreIntensity()
	For X = 0 to MaxLights
		Intensitymem(X) = Inserts(X).Intensity
	Next
End Sub

Dim PulseAmount
PulseAmount = 1
Sub TiPulser_Timer()
	For X = 0 to 63
		If IsPulsing(x) = 1 Then
			LightArray(x, 0).Intensity = PulseAmount
		End If
	Next
	PulseAmount = PulseAmount + (0.25 * PulseDir)
	If PulseAmount > 14 OR PulseAmount < 1 Then PulseDir = PulseDir * -1
End Sub

Sub BackFlash()
	'EP- Do some fancy flashing stuff here
End Sub

Sub RightFlashTimer_Timer()
	RightFlashTimer.Enabled=False
	RightFlash.State=LightStateOff: MidFlash.State=LightStateOff: LeftFlash.State=LightStateOff
	Light80.State=LightStateOff: Light81.State=LightStateOff
End Sub

Sub MidFlashEffect()
	MidFlash.TimerEnabled=True: MidFlash.State=LightStateBlinking
End Sub

Sub MidFlash_Timer()
	MidFlash.TimerEnabled=False: MidFlash.State=LightStateOff
End Sub

Sub Trigger5_Hit()
	With ActiveBall : .X = .X-10 : .Y = .Y-150 : .Z = 80 : .VelZ = -2 : End With
End Sub

Sub FlashSword()
	LightArray(34, 1) = 3
	LightArray(35, 1) = 3
	Light35.TimerEnabled = 1
End Sub

Sub Light35_Timer()								'How long to flash the sword
	Me.TimerEnabled=False
	LightArray(34, 1) = 0
	LightArray(35, 1) = 0
End Sub

'ball save------------------
Sub BallSaveTimer_Timer()
	BallSaveTimer.Enabled=False
	BallSave=0
	LightArray(22, 1) = 0
End Sub

Sub CheckInsult()
	If EscapeActive=1 Then

	Else
		PlaySound "tink"
'		AdvanceModeCount							'EP- I'm moving this so the modes change when the bumpers get hit
		AddScore(2500)
		DisplayFlushQueue
		DisplayQueueScreen "  "&II&" "&NN&" "&SS&" "&UU&" "&LL&" "&TT&"   ","  SPELL INSULT  ",0,0,0,1000,False,""
		DisplayQueueScreen "  "&II&" "&NN&" "&SS&" "&UU&" "&LL&" "&TT&"   "," TO LIGHT MODES ",0,0,0,1500,True,""
		If (II="I") And (NN="N") And (SS="S") And (UU="U") And (LL="L") And (TT="T") Then ModesLit() 'should be here
	End If
End Sub

Sub InsultBonus()
	DisplayFlushQueue
	DisplayQueueScreen " ADD TO JACKPOT ","      25K      ",0,0,0,500,True,""
	PlaySound "clack": PlaySound "multihit1"
	Jackpot=Jackpot+25000
	DisplayScoreQueue 0,0
End Sub

'***********************
'*	Targets
'***********************

Sub IITarg_Hit()
	Set BallSM = ActiveBall
	Select Case Status
	Case "Normal"
		If II="*" Then
			Select Case Play_Mode
			Case "Easy"
				II = "I"
				NN = "N"
				SS = "S"
				LightArray(57, 1) = 1
				LightArray(58, 1) = 1
			Case Else
				II = "I"
			End Select
			EndCurrentInsult
			PlaySound "insultI"
		End If
		LightArray(56, 1) = 1
		CheckInsult()
		AddBonus(5000)
	Case "VoodooMagic","Spitting","HighDive","DaintyLady","MonkeyCombat","GraveDigger"
		InsultBonus()
	Case "SwordMaster"
		If II="*" Then
			II="I"
			LastInsult="I"
			LightArray(56, 1) = 0
			SetPulse 56, -1
			CheckSword()
		End If
	Case "TreasureHunt"
		If LastInsult="I" And TH_ToDo=3 Then
			LightArray(56, 1) = 1
			TreasureHunt_Three()
		End If
	Case "GoEscape"
		If EscapeSwitch(21)=0 Then
			LightArray(56, 1) = 1
			SwitchNumber=21
			SwitchOn()
		End If
	End Select
End Sub

Sub NNTarg_Hit()
	Set BallSM = ActiveBall
	Select Case Status
	Case "Normal"
		If NN="*" Then
			Select Case Play_Mode
			Case "Easy"
				II = "I"
				NN = "N"
				SS = "S"
				LightArray(56, 1) = 1
				LightArray(58, 1) = 1
			Case Else
				NN = "N"
			End Select
			EndCurrentInsult
			PlaySound "insultN"
		End If
		LightArray(57, 1) = 1
		CheckInsult()
		AddBonus(5000)
	Case "VoodooMagic","Spitting","HighDive","DaintyLady","MonkeyCombat","GraveDigger"
		InsultBonus
	Case "SwordMaster"
		If NN="*" Then
			NN="N": LastInsult="N"
			LightArray(57, 1) = 0
			SetPulse 57, -1
			CheckSword()
		End If
	Case "TreasureHunt"
		If LastInsult="N" And TH_ToDo=3 Then
			LightArray(57, 1) = 1
			TreasureHunt_Three()
		End If
	Case "GoEscape"
		If EscapeSwitch(22)=0 Then
			LightArray(57, 1) = 1
			SwitchNumber=22
			SwitchOn()
		End If
	End Select
End Sub

Sub SSTarg_Hit()
	Set BallSM = ActiveBall
	Select Case Status
	Case "Normal"
		If SS="*" Then
			Select Case Play_Mode
			Case "Easy"
				II = "I"
				NN = "N"
				SS = "S"
				LightArray(56, 1) = 1
				LightArray(57, 1) = 1
			Case Else
				SS = "S"
			End Select
			EndCurrentInsult
			PlaySound "insultS"
		End if
		LightArray(58, 1) = 1
		CheckInsult()
		AddBonus(5000)
	Case "VoodooMagic","Spitting","HighDive","DaintyLady","MonkeyCombat","GraveDigger"
		InsultBonus()
	Case "SwordMaster"
		If SS="*" Then
			SS="S": LastInsult="S"
			LightArray(58, 1) = 0
			SetPulse 58, -1
			CheckSword()
		End If
	Case "TreasureHunt"
		If LastInsult="S" And TH_ToDo=3 Then
			LightArray(58, 1) = 1
			TreasureHunt_Three()
		End If
	Case "GoEscape"
		If EscapeSwitch(23)=0 Then
			LightArray(58, 1) = 1
			SwitchNumber=23
			SwitchOn()
		End If
	End Select
End Sub

Sub UUTarg_Hit()
	Set BallSM = ActiveBall
	Select Case Status
	Case "Normal"
		If UU="*" Then
			Select Case Play_Mode
			Case "Easy"
				UU = "U"
				LL = "L"
				TT = "T"
				LightArray(60, 1) = 1
				LightArray(61, 1) = 1
			Case Else
				UU = "U"
			End Select
			EndCurrentInsult
			PlaySound "insultU"
		End If
		LightArray(59, 1) = 1
		CheckInsult()
		AddBonus(5000)
	Case "VoodooMagic","Spitting","HighDive","DaintyLady","MonkeyCombat","GraveDigger"
		InsultBonus
	Case "SwordMaster"
		If UU="*" Then
			UU="U"
			LastInsult="U"
			LightArray(59, 1) = 0
			SetPulse 59, -1
			CheckSword()
		End If
	Case "TreasureHunt"
		If LastInsult="U" And TH_ToDo=3 Then
			LightArray(59, 1) = 1
			TreasureHunt_Three()
		End If
	Case "GoEscape"
		If EscapeSwitch(24)=0 Then
			LightArray(59, 1) = 1
			SwitchNumber=24
			SwitchOn()
		End If
	End Select
End Sub

Sub LLTarg_Hit()
	Set BallSM = ActiveBall
	Select Case Status
	Case "Normal"
		If LL="*" Then
			Select Case Play_Mode
			Case "Easy"
				UU = "U"
				LL = "L"
				TT = "T"
				LightArray(59, 1) = 1
				LightArray(61, 1) = 1
			Case Else
				LL="L"
			End Select
			EndCurrentInsult
			PlaySound "insultLL"
		End If
		LightArray(60, 1) = 1
		CheckInsult()
		AddBonus(5000)
	Case "VoodooMagic","Spitting","HighDive","DaintyLady","MonkeyCombat","GraveDigger"
		InsultBonus()
	Case "SwordMaster"
		If LL="*" Then
		LL="L": LastInsult="L"
			LightArray(60, 1) = 0
			SetPulse 60, -1
			CheckSword()
		End If
	Case "TreasureHunt"
		If LastInsult="L" And TH_ToDo=3 Then
			LightArray(60, 1) = 1
			TreasureHunt_Three()
		End If
	Case "GoEscape"
		If EscapeSwitch(25)=0 Then
			LightArray(60, 1) = 1
			SwitchNumber=25
			SwitchOn()
		End If
	End Select
End Sub

Sub TTTarg_Hit()
	Set BallSM = ActiveBall
	Select Case Status
	Case "Normal"
		If TT="*" Then
			Select Case Play_Mode
			Case "Easy"
				UU = "U"
				LL = "L"
				TT = "T"
				LightArray(59, 1) = 1
				LightArray(60, 1) = 1
			Case Else
				TT="T"
			End Select
			EndCurrentInsult
			PlaySound "insultT"
		End If
		LightArray(61, 1) = 1
		CheckInsult()
		AddBonus(5000)
	Case "VoodooMagic","Spitting","HighDive","DaintyLady","MonkeyCombat","GraveDigger"
		InsultBonus()
	Case "SwordMaster"
		If TT="*" Then
			TT="T": LastInsult="T"
			LightArray(61, 1) = 0
			SetPulse 61, -1
			CheckSword()
		End If
	Case "TreasureHunt"
		If LastInsult="T" And TH_ToDo=3 Then
			LightArray(61, 1) = 1
			TreasureHunt_Three()
		End If
	Case "GoEscape"
		If EscapeSwitch(26)=0 Then
			LightArray(61, 1) = 1
			SwitchNumber=26
			SwitchOn()
		End If
	End Select
End Sub

Sub EndCurrentInsult()
	StopSound "insultI"
	StopSound "insultN"
	StopSound "insultS"
	StopSound "insultU"
	StopSound "insultLL"
	StopSound "insultT"
	StopSound "imildlyinsulted"
End Sub

'************ End Targets ***************


' ****************************** MUSIC *************************************

Sub Table1_MusicDone()	' restarts the music
	PlayMusic Song
End Sub

'********************** End Music*************
'********************* stats and operator menu ****************************
Sub LoadStats()
	BallsPerGame=LoadValue("EFMI","Nb_Balls")
	If BallsPerGame="" Then BallsPerGame=3
	Value=LoadValue("EFMI","Mode")
	Play_Mode="Medium": If (Value<>"") Then Play_Mode=Value
	gsHighScore(0)=LoadValue("EFMI","Champion")
	gsHighScoreName(0)=LoadValue("EFMI","ChampionName")
	gsHighScore(1)=LoadValue("EFMI","HiScore1")
	gsHighScoreName(1)=LoadValue("EFMI","HiScore1Name")
	gsHighScore(2)=LoadValue("EFMI","HiScore2")
	gsHighScoreName(2)=LoadValue("EFMI","HiScore2Name")
	gsHighScore(3)=LoadValue("EFMI","HiScore3")
	gsHighScoreName(3)=LoadValue("EFMI","HiScore3Name")
	gsHighScore(4)=LoadValue("EFMI","HiScore4")
	gsHighScoreName(4)=LoadValue("EFMI","HiScore4Name")
	gsLoopChamp=LoadValue("EFMI","LoopChamp")
	gsLoopChampName=LoadValue("EFMI","LoopChampName")
	' If one data corrupt reset all variables...
	If gsHighScore(0)="" Or gsHighScoreName(1)="" Or gsHighScoreName(2)="" Or gsHighScoreName(3)="" Or gsHighScoreName(4)="" Or gsLoopChampName="" Or (IsNumeric(gsHighScore(0))=False) Or (IsNumeric(gsHighScore(1))=False) Or (IsNumeric(gsHighScore(2))=False) Or (IsNumeric(gsHighScore(3))=False) Or (IsNumeric(gsHighScore(4))=False) Or (IsNumeric(gsLoopChamp)=False)Then
		ResetHiScores
		Exit Sub
	End If
	gsHighScore(0)=CDbl(gsHighScore(0))
	gsHighScore(1)=CDbl(gsHighScore(1))
	gsHighScore(2)=CDbl(gsHighScore(2))
	gsHighScore(3)=CDbl(gsHighScore(3))
	gsHighScore(4)=CDbl(gsHighScore(4))
	gsLoopChamp=CInt(gsLoopChamp)
	Value=LoadValue("EFMI","Nudge")
	NudgeAllowed="No"
	If Value<>"" Then NudgeAllowed=Value
End Sub

Sub ResetHiScores()
	gsHighScoreName(0)="IAN": gsHighScore(0)=120000000
	gsHighScoreName(1)="MON": gsHighScore(1)=80000000
	gsHighScoreName(2)="KEY": gsHighScore(2)=60000000
	gsHighScoreName(3)="ISL": gsHighScore(3)=40000000
	gsHighScoreName(4)="AND": gsHighScore(4)=20000000
	gsLoopChampName="IAN": gsLoopChamp=1
End Sub

Sub SaveHiScore()
	SaveValue "EFMI","Champion",gsHighScore(0): SaveValue "EFMI","ChampionName",gsHighScoreName(0)
	SaveValue "EFMI","HiScore1",gsHighScore(1): SaveValue "EFMI","HiScore1Name",gsHighScoreName(1)
	SaveValue "EFMI","HiScore2",gsHighScore(2): SaveValue "EFMI","HiScore2Name",gsHighScoreName(2)
	SaveValue "EFMI","HiScore3",gsHighScore(3): SaveValue "EFMI","HiScore3Name",gsHighScoreName(3)
	SaveValue "EFMI","HiScore4",gsHighScore(4): SaveValue "EFMI","HiScore4Name",gsHighScoreName(4)
	SaveValue "EFMI","LoopChamp",gsLoopChamp: SaveValue "EFMI","LoopChampName",gsLoopChampName
End Sub

Sub OperatorMenu()
	TextBox1.Text="Left Flipper = Next"& vbNewLine&"Right Flipper = Change"& vbNewLine&"1 = Set"&vbNewLine&"6 = EXIT"
	Dim TempTopStr, TempBottomStr
	Option_Set=" "
	If MenuIndex > 4 Then MenuIndex=1
	If MenuLineIndex(1)>2 Then MenuLineIndex(1)=1
	If MenuLineIndex(2)>3 Then MenuLineIndex(2)=1
	If MenuLineIndex(3)>2 Then MenuLineIndex(3)=1
	If MenuLineIndex(4)>2 Then MenuLineIndex(4)=1
	Select Case MenuIndex
		Case 1
			TempTopStr="NUMBER OF BALLS"
			Select Case MenuLineIndex(1)
				Case 1
				If BallsPerGame=3 Then Option_Set="*"
				TempBottomStr="     " & Option_Set & " 3     "
				Case 2
				If BallsPerGame=5 Then Option_Set="*"
				TempBottomStr="     " & Option_Set & " 5     "
			End Select
		Case 2
			TempTopStr="   PLAY MODE  "
			Select Case MenuLineIndex(2)
				Case 1
				If Play_Mode="Easy" Then Option_Set="*"
				TempBottomStr="    " & Option_Set & " EASY   "
				Case 2
				If Play_Mode="Medium" Then Option_Set="*"
				TempBottomStr="   " & Option_Set & " MEDIUM   "
				Case 3
				If Play_Mode="Hard" Then Option_Set="*"
				TempBottomStr="    " & Option_Set & " HARD    "
			End Select
		Case 3
			TempTopStr="RESET HI SCORES"
			Select Case MenuLineIndex(3)
				Case 1
				If Reset_HS="NO" Then Option_Set="*"
				TempBottomStr="      " & Option_Set & " NO    "
				Case 2
				If Reset_HS="YES" Then Option_Set="*"
				TempBottomStr="     " & Option_Set & " YES   "
			End Select
		Case 4
			TempTopStr="NUDGING ALLOWED"
			Select Case MenuLineIndex(4)
				Case 1
				If NudgeAllowed="No" Then Option_Set="*"
				TempBottomStr="      " & Option_Set & " NO    "
				Case 2
				If NudgeAllowed="Yes" Then Option_Set="*"
				TempBottomStr="     " & Option_Set & " YES   "
			End Select
	End Select
	DisplayFlushQueue
	DisplayQueueScreen TempTopStr,TempBottomStr,0,0,0,1000,False,""
End Sub

Sub OperatorMenu_End()
	If Reset_HS = "YES" Then
		ResetHiScores: SaveHiScore
	End If
	SaveValue "EFMI","Nb_Balls",BallsPerGame: SaveValue "EFMI","Mode",Play_Mode: SaveValue "EFMI","Nudge",NudgeAllowed
	PlaySound "coins"
	InAttract = 1
End Sub

Sub OperatorMenuValidate
	Select Case MenuIndex
		Case 1 				' ball numbers
			BallsPerGame=3
			If MenuLineIndex(1)=2 Then BallsPerGame=5
		Case 2 			' difficulty
			If MenuLineIndex(2)=1 Then Play_Mode="Easy"
			If MenuLineIndex(2)=2 Then Play_Mode="Medium"
			If MenuLineIndex(2)=3 Then Play_Mode="Hard"
		Case 3	 			' resetHi Scores
			If MenuLineIndex(3)=2 Then Reset_HS="YES"
		Case 4 				' nudging
			NudgeAllowed="No"
			If MenuLineIndex(4)=2 Then NudgeAllowed="Yes"
	End Select
	OperatorMenu
End Sub

Sub OperatorMenuTimer_Timer()
	OperatorMenuTimer.Enabled=False
	OperatorMenu
End Sub

'********************** End Operator Stuff***************

Sub GoMystery()
	DisplayFlushQueue
	PlaySound "asyouwish"
	DisplayQueueScreen " INTERNATIONAL  "," HOUSE OF MOJO  ",6,7,0,1500,True,""
	DisplayQueueScreen " PRESS FLIPPER  "," TO CHOOSE ITEM ",3,3,0,1500,True,""
	MysteryWaitTimer.Enabled=True
	LightArray(46, 1) = 3
End Sub

Sub MysteryWaitTimer_Timer()
	MysteryWaitTimer.Enabled=False
	DisplayFlushQueue
	MysteryTimer.Enabled=True
End Sub

Sub MysteryTimer_Timer()
	MysteryTimer.Enabled=False
	MysteryActive=True
	PlaySound "click1"
	Mystery=round(Rnd*10)
	Select Case Mystery
		Case 0: TextBoxTop.Text="   LIT  EXTRA   ": TextBoxBottom.Text="      BALL      "
		Case 1: TextBoxTop.Text="    NOTHING     ": TextBoxBottom.Text="      HERE      "
		Case 2: TextBoxTop.Text="     BONUS      ": TextBoxBottom.Text="   MULTIPLIER   "
		Case 3: TextBoxTop.Text="    OUTLANES    ": TextBoxBottom.Text="      LIT       "
		Case 4: TextBoxTop.Text="     EXTRA      ": TextBoxBottom.Text="      BALL      "
		Case 5: TextBoxTop.Text="  GROG MACHINE  ": TextBoxBottom.Text="    IS OPEN     "
		Case 6: TextBoxTop.Text="      + 50      ": TextBoxBottom.Text="     BARRELS    "
		Case 7: TextBoxTop.Text="     SMALL      ": TextBoxBottom.Text="     POINTS     "
		Case 8: TextBoxTop.Text="     BONUS      ": TextBoxBottom.Text="      HELD      "
		Case 9: TextBoxTop.Text="      BIG       ": TextBoxBottom.Text="     POINTS     "
	End Select
	MysteryTimer.Enabled=True
End Sub

Sub MysteryProcessKey(keycode)
	Select Case (Keycode)
	Case LeftflipperKey
		If MysteryActive=True Then
			CheckMystery
			MysteryActive=False
		End If
	Case RightflipperKey
		If MysteryActive=True Then
			CheckMystery
			MysteryActive=False
		End If
	End Select
End Sub

Sub CheckMystery()
	MysteryTimer.Enabled=False
	DisplayFlushQueue
	PlaySound "caphit"
	Select Case Mystery
	Case 0
		DisplayQueueScreen "   LIT  EXTRA   ","      BALL      ",8,8,0,1500,False,"knocking"
		DisplayQueueScreen "   SHOOT THE    ","   SCUMM BAR    ",3,3,0,1500,False,""
		EB=1
		LightArray(20, 1) = 1
		ScummOn()
	Case 1
		DisplayQueueScreen "    NOTHING     ","      HERE      ",8,8,0,1500,False,"whatsthis"
		DisplayQueueScreen "                ","        0       ",0,3,0,1500,False,""
	Case 2
		DisplayQueueScreen "     BONUS      ","   MULTIPLIER   ",8,8,0,1500,False,""
		AddMulti()
		DisplayQueueScreen "     BONUS      ","      X "&BonusMultiplier&" ",0,3,0,1500,False,""
	Case 3
		DisplayQueueScreen "    OUTLANES    ","      LIT       ",8,8,0,1500,False,"init5"
		LightArray(1, 1) = 1
		LightArray(3, 1) = 1
	Case 4
		DisplayQueueScreen "     EXTRA      ","      BALL      ",8,8,0,1500,False,"thumb"
		ExtraBall()
	Case 5
		GrogHits=2
		CheckGrog()
	Case 6
		DisplayQueueScreen "      + 50      ","     BARRELS    ",8,8,0,1000,False,"wallop"
		Barrels=Barrels+50
		DisplayQueueScreen "      "&Barrels&"       "," BARRELS BUSTED ",3,3,0,1000,False,""
	Case 7
		x=0
		While x < 100000
			x=round(rnd*200000)
		Wend
		DisplayQueueScreen " SMALL POINTS ","  "&FormatScore(x)&"    ",8,8,0,1500,False,"coin"
		AddScore(x)
	Case 8
		DisplayQueueScreen "     BONUS      ","      HELD      ",8,8,0,1500,False,"bellring"
		BonusHeld=1
	Case 9
		x=0
		While x < 1000000
			x=round(rnd*3000000)
		Wend
		DisplayQueueScreen "   BIG POINTS  ","  "&FormatScore(x)&" ",8,8,0,1500,False,"coins"
		AddScore(x)
	End Select
	VoodooCount=0
	TiKicker6.Enabled=True
End Sub

Sub NormalVoodoo()
	PlaySound "voodoohit"
	EndCurrentInsult()
	'EP- Maybe Add in a light effect for the VooDoo lady
	VoodooCount=VoodooCount+1
	AddBonus(5000)
	If VoodooCount=1 Then
		PlaySound "ahguybrush"
		DisplayQueueScreen "  "&VoodooCount&" VOODOO HITS ","  "&(3-VoodooCount)&" FOR MYSTERY ",6,7,0,1500,True,""
		AddScore(75000)
	End If
	If VoodooCount=2 Then
		DisplayQueueScreen "  "&VoodooCount&" VOODOO HITS ","  "&(3-VoodooCount)&" FOR MYSTERY ",6,7,0,1500,True,""
		PlaySound "iknew"
		AddScore(150000)
	End If
	If VoodooCount=3 Then
		DisplayQueueScreen "  "&VoodooCount&" VOODOO HITS "," MYSTERY ACTIVE ",3,8,0,1500,True,"efmi_fast3"
		Kicker6.Enabled=True
		PlaySound "howcani"
		LightArray(46, 1) = 1
		AddScore(250000)
	End If
	DisplayScoreQueue 2,1
End Sub

Sub TiKicker6_Timer()
	TiKicker6.Enabled=False
	VooDooPopUp.IsDropped=True
	VooDooMag.MagnetOn = 0
	Kicker6.Enabled=False
	LightArray(46, 1) = 0
	If Status="TreasureHunt" Then Exit Sub
	DisplayScoreQueue 1,2
	PlaySound "illbeherelater"
	Song="EFMI_MonkeyTheme.mp3": PlayMusic Song
End Sub

Sub Kicker6_Hit()
	VooDooMag.AddBall ActiveBall
	Set BallMover2 = ActiveBall
	LightArray(46, 1) = 0
	Select Case Status
	Case "Normal"
		PlaySound "voodoohit"
		PlaySound "monkeytargup"
		EndMusic()
		VooDooPopUp.IsDropped=False
		GoMystery
	Case "TreasureHunt"
		PlaySound "voodoohit"
		TreasureHunt_Four()
	Case "VoodooMagic"
'		VoodooItemHit(ActiveBall.ID)
	Case Else
		Kicker6.Enabled=False
		VooDooMag.magnetOn = 0
		PlaySound "ballinr"
	End Select
End Sub

Sub Kicker6_UnHit()
	VooDooMag.RemoveBall ActiveBall
	Set VooDooChecker = Nothing
End Sub

'*************** grog machine (collect barrels) *************

Sub CheckGrog()
	If Status="Normal" Then
		GrogHits=GrogHits+1
		DisplayFlushQueue()
		Select Case GrogHits
		Case 1
			DisplayQueueScreen "       1        ","   LEFT LOOP    ",2,1,0,1200,False,"pour"
			DisplayQueueScreen " 2 MORE TO OPEN ","  GROG MACHINE  ",3,3,0,1200,True,""
			LightArray(43, 1) = 1
			AddScore(12000)
		Case 2
			DisplayQueueScreen "       2        ","   LEFT LOOPS   ",2,1,0,1200,False,"pour"
			DisplayQueueScreen " 1 MORE TO OPEN ","  GROG MACHINE  ",3,3,0,1200,True,""
			LightArray(44, 1) = 1
			AddScore(25000)
		Case 3
			DisplayQueueScreen "       3        ","   LEFT LOOPS   ",2,1,0,1200,False,"pour"
			DisplayQueueScreen "  GROG MACHINE  ","    IS OPEN     ",8,8,0,1500,True,"efmi_fast3"
'			If Status = "Normal" Then GrogKicker.Enabled = 1
			GrogLight(1)
			LightArray(43, 1) = 2
			LightArray(44, 1) = 2
			LightArray(45, 1) = 2
		Case 4
			DisplayQueueScreen "  GROG MACHINE  ","     IS OPEN    ",3,3,0,1500,False,"init5"
			If Status = "Normal" Then GrogKicker.Enabled = 1
			GrogHits=3
			AddBonus(5000)
		End Select
	End If
	DisplayScoreQueue 1,2
End Sub

Sub GoGrog()
	EndMusic()
	GrogLight(-1)
	GiGrogTimer.Enabled = 0
	GrogTimer.Enabled=True
	GrogKicker.Enabled=False
	GrogHits=0
	DisplayFlushQueue()
	PlaySound "grog"
	DisplayQueueScreen "  GROG MACHINE  ","  GROG MACHINE  ",4,4,0,1000,False,""
	DisplayQueueScreen "  GROG MACHINE  ","  GROG MACHINE  ",3,3,0,1000,False,""
	LightArray(41, 1) = 1
	LightArray(43, 1) = 0
	LightArray(44, 1) = 0
	LightArray(45, 1) = 0
End Sub

Sub GrogTimer_Timer() 'delay before counting up the barrels
	GrogTimer.Enabled=False
	BarrelCountTimer.Enabled=True 'start the counter
	DisplayFlushQueue()
	DisplayTimer.Enabled=False
End Sub

Sub BarrelCountTimer_Timer()
	BarrelCountTimer.Enabled=False
	GrogTotal=GrogTotal+BarrelScore
	PlaySound "gulp"
	TextBoxTop.Text="   " & Barrels & " BARRELS": TextBoxBottom.Text= "    + "& GrogTotal
	Barrels=Barrels-1
	If Barrels > 0 Then
		BarrelCountTimer.Enabled=True
	Else
		BarrelCountTimer.Enabled=False
		If GrogTotal<10000 Then GrogTotal=10000
		DisplayTimer.Enabled=True
		DisplayQueueScreen "   GROG TOTAL   "," = "&FormatScore(GrogTotal)&" ",0,8,0,1750,False,""
		PlaySound "burp2"
		AddScore(GrogTotal)
		GrogEndTimer.Enabled=True
	End If
End Sub

Sub GrogEndTimer_Timer()
	GrogEndTimer.Enabled=False
	PlaySound "ballinr"
	GrogKicker.Kick 4,22
	DisplayScoreQueue 2,1
	GrogLight(-1)
	GiGrogTimer.Enabled = 0
	GrogKicker.Enabled=False
	LightArray(41, 1) = 0
	GrogTotal=0
	Song="EFMI_MonkeyTheme.mp3"
	PlayMusic Song
	DisplayScoreQueue 0,0
End Sub

'************************************ MODES ************************************************
'1 = voodoo magic
'2 = spitting comp
'3 = high dive
'4 = swordsmaster
'5 = dainty lady
'6 = treasure hunt
'7 = grave digger
'8 = monkey combat
'9 = escape mode

Dim ModePicking
Sub ScummKicker_Hit()
	'EP- Turn off grog trigger?
	ScummMag.AddBall ActiveBall
	If ScummMag.MagnetOn = 1 Then
		ScummTrapper.IsDropped = 0
		PlaySound "ballinr"
		If EB=1 Then
			ExtraBall
		Else
			If Play_Mode = "Easy" Then ModePicking = 1
			PlaySound "stayoutkitchen"
			SeqModes.Play SeqRandom ,6,,2500
			If ModePicking = 1 Then Exit Sub
			RestOfScumm()
		End If
	End if
End Sub

Sub PickMode(dir)
	If dir <> 0 Then
		AdvanceModeCount()
	Else
		RestOfScumm()
		ModePicking = 0
	End If
End Sub

Sub ScummKicker_UnHit()
	ScummMag.RemoveBall ActiveBall
End Sub

Sub ExtraBall()
	EB=0
	EndMusic()
	DisplayFlushQueue
	DisplayQueueScreen "    E X T R A   ","     B A L L    ",1,2,0,500,False,"explode1"
	DisplayQueueScreen "    E X T R A   ","     B A L L    ",3,3,0,500,False,""
	DisplayQueueScreen "    E X T R A   ","     B A L L    ",8,8,0,1500,False,""
	Attract1.Play SeqMiddleOutVertOn ,5,,500
	Attract1.Play SeqMiddleOutVertOff ,5,,500
	Attract1.Play SeqRandom ,6,,1500
	ExtraBalls=ExtraBalls+1
	EBScoredThisBall=1
	PlaySound "rollthroughthegatesofhell"
	LightArray(40, 1) = 0
	ExtraBallTimer.Enabled=True
End Sub

Sub ExtraBallTimer_Timer()
	ExtraBallTimer.Enabled=False
	ScummMag.MagnetOn = 0
	ScummTrapper.IsDropped = 1
	If LightArray(62, 1) = 1 Then
		RestOfScumm()
	Else
		PlayMusic Song
		PlaySound "ballinr": PlaySound "doorclose"
		DisplayScoreQueue 1,2
	End If
End Sub

Sub RestOfScumm()
	If Status="Normal" Then
		InsultOff()
		CenterArrowsOff()
		ModesStarted=ModesStarted+1
		BallSaveTimer.Enabled=False
		BallSave=0
		LightArray(22, 1) = 0
		DisplayFlushQueue()
		If ModesStarted < 9 Then
			ModeIntroTimer.Enabled=True
			DisplayQueueScreen "      MODE      ","     STARTED    ",0,0,0,1000,False,""
			PlaySound "monkeyend"
			If Bananas>0 Then
				DisplayQueueScreen "  "&Bananas&" BANANAS ","  + "&Bananas&" SECONDS ",0,0,0,1000,False,"parrotcall"
				AddScore(100000*Bananas)
			End If
			EndMusic
		Else 'paranoia check - shouldn't get here
			ModesStarted=8
			DisplayQueueScreen "  ESCAPE MODE   ","     IS LIT     ",8,8,0,1000,False,"bigstep"
			DisplayQueueScreen "      HIT       ","    LE CHUCK    ",3,3,0,1000,False,"init5"
			DisplayScoreQueue 1,2
			ScummMag.MagnetOn = 0
			ScummTrapper.IsDropped = 1
		End If
	Else
' only possible case: just before multiball we have lit one mode...
		ScummMag.MagnetOn = 0
		ScummTrapper.IsDropped = 1
	End If
End Sub

Sub ModeIntroTimer_Timer
	ModeIntroTimer.Enabled=False
	TiScummKicker.Enabled = True
	LightArray(43, 1) = 0
	LightArray(44, 1) = 0
	LightArray(45, 1) = 0
	GrogKicker.Enabled=False 'turn off the grog machine, relight it after mode if needed
	GrogLight(-1)
	GiGrogTimer.Enabled = 0
	LightArray(31, 1) = 0
	GoModes()
End Sub

Sub AdvanceModeCount() 'remember these are NOT the correect order of the modes, just the lights ;)
	X = 0
	CancelModeLights() 'turn them all off first
	ModeCount = ModeCount + 1
	If ModeCount>8 Then ModeCount = 1
	Do while X <> 10
		If ModesGet(Modecount) = 1 Then
			ModeCount = ModeCount + 1
			X = X + 1
		Else
			X = 10
		End If
	Loop
	Select Case ModeCount
	Case 1							'VooDoo
		LightArray(8, 1) = 1
	Case 2							'High Dive
		LightArray(9, 1) = 1
	Case 3							'Spit
		LightArray(10, 1) = 1
	Case 4							'Dainty Lady
		LightArray(11, 1) = 1
	Case 5							'Sword Master
		LightArray(12, 1) = 1
	Case 6							'Treasure Hunt
		LightArray(6, 1) = 1
		LightArray(25, 1) = 1
	Case 7							'Monkey Battle
		LightArray(0, 1) = 1
	Case 8
		LightArray(5, 1) = 1		'Grave Digger
	End Select
End Sub

Sub GoModes()
	If HeadHits>3 Then 'if the mouth is open Then stop shots getting in it
		MoveMouthGuard(1)
		PlaySound "solon"
	End If
	Select Case ModeCount
	Case 1	 'The real order...
		LightArray(8, 1) = 1
		SetPulse 8, 1
		ModesGet(ModeCount)=1
		VooDooMagic()
	Case 2
		LightArray(9, 1) = 1
		SetPulse 9, 1
		ModesGet(ModeCount)=1
		HighDive()
	Case 3
		LightArray(10, 1) = 1
		SetPulse 10, 1
		ModesGet(ModeCount)=1
		SpittingCompetition()
	Case 4
		LightArray(11, 1) = 1
		SetPulse 11, 1
		ModesGet(ModeCount)=1
		DaintyLady()
	Case 5
		LightArray(12, 1) = 1
		SetPulse 12, 1
		ModesGet(ModeCount)=1
		SwordMaster()
	Case 6
		LightArray(6, 1) = 1
		SetPulse 6, 1
		LightArray(25, 1) = 1
		SetPulse 25, 1
		TiScummKicker.Interval = TiScummKicker.Interval + 2500
		ModesGet(ModeCount)=1
		TreasureHunt()
	Case 7
		LightArray(0, 1) = 1
		SetPulse 0, 1
		ModesGet(ModeCount)=1
		MadMonkeyCombat()
	Case 8
		LightArray(5, 1) = 1
		SetPulse 5, 1
		ModesGet(ModeCount)=1
		GraveDigger()
		TiScummKicker.Interval = TiScummKicker.Interval + 1000
	End Select
End Sub

Sub TiScummKicker_Timer()
	DisplayScoreQueue 2,1
	LightArray(62, 1) = 0
	ScummOff()
	TiScummKicker.Enabled = False
	ActiveMonkeyBeat()
	Kicker6.Enabled=False
	VooDooMag.MagnetOn = 0
	LightArray(46, 1) = 0		' JPR
	Select Case Status
	Case "VoodooMagic"
		Song="EFMI_Voodoo.mp3": PlayMusic Song
		ScummMag.MagnetOn = 0
		ScummTrapper.IsDropped = 1
		PlaySound "ballinr": PlaySound "doorclose"
	Case "Spitting"
		Song="EFMI_Spit.mp3": PlayMusic Song
		GiGrogTimer.Enabled = 1
		ModeTimeLeft=11 + Bananas
		ModeLengthTimer.Enabled=True
		ScummMag.MagnetOn = 0
		ScummTrapper.IsDropped = 1
		PlaySound "ballinr": PlaySound "doorclose"
	Case "HighDive"
		Song="EFMI_Dive.mp3": PlayMusic Song
		GiGrogTimer.Enabled = 1
		ModeTimeLeft=11 + Bananas
		ModeLengthTimer.Enabled=True
		ScummMag.MagnetOn = 0
		ScummTrapper.IsDropped = 1
		PlaySound "ballinr": PlaySound "doorclose"
	Case "SwordMaster"
		Dim tempballs
		tempballs = ScummMag.Balls
		'ScummKicker.DestroyBall
		Set BallSM = tempballs(0)
		Song="EFMI_Sword.mp3": PlayMusic Song
		ModeTimeLeft=41 + Bananas
		ModeLengthTimer.Enabled=True
		ScummMag.MagnetOn = 0
		ScummTrapper.IsDropped = 1
		PlaySound "ballinr": PlaySound "doorclose"
	Case "DaintyLady"
		Song="EFMI_Ship.mp3": PlayMusic Song
		ModeTimeLeft=Round((26 + Bananas) * CoeffTimer)
		ModeLengthTimer.Enabled=True
		ScummMag.MagnetOn = 0
		ScummTrapper.IsDropped = 1
		PlaySound "ballinr": PlaySound "doorclose"
	Case "MonkeyCombat"
		Song="EFMI_Ultimate.mp3": PlayMusic Song
		ModeTimeLeft=61 + Bananas
		If Play_Mode="Hard" Then ModeTimeleft=Round(ModeTimeLeft*CoeffTimer)				' ModeLengthTimer is enabled with kickout in Mode Script
	Case "TreasureHunt"
		Song="EFMI_Spit.mp3": PlayMusic Song
		ModeTimeLeft=Round((30 + Bananas)*CoeffTimer)
		ModeLengthTimer.Enabled=True
		ScummMag.MagnetOn = 0
		ScummTrapper.IsDropped = 1
		PlaySound "ballinr": PlaySound "doorclose"
	Case "GraveDigger"
		Song="EFMI_GraveDigger.mp3": PlayMusic Song
		ModeTimeLeft=Round((30 + Bananas)*CoeffTimer)
		ModeLengthTimer.Enabled=True
		ScummMag.MagnetOn = 0
		ScummTrapper.IsDropped = 1
		PlaySound "ballinr": PlaySound "doorclose"
	End Select
	If Status <> "Normal" Then
		BallSaveTimer.Enabled=True 				'turn this back on!
		BallSave=1
		LightArray(22, 1) = 2					'just to make sure : O
	End If
End Sub

Sub ModeLengthTimer_Timer()
	ModeLengthTimer.Enabled=False
	PlaySound "tick"
	ModeTimeLeft = ModeTimeLeft - 1
	If ModeTimeLeft=0 Then
		If Status="GoEscape" Then 'escape mode over
			BallSave=0
			BallSaveTimer.Enabled=False
			LightArray(22, 1) = 0
			FlipOkay=0
			LeftFlipper.RotateToStart
			RightFlipper.RotateToStart
			PlaySound "Flipperdown"
			PlaySound "Flapopen"
			EndMusic()
			Attract1.StopPlay
			PlaySound "clockchime"
		Else
			If BallsinPlay>0 Then GoModeTotal 'make sure has not been eneded with ball drain
			PlaySound "clockstrike"
		End If
	Else
		TextBoxBottom.Text = "   TIME = "&ModeTimeLeft&"  "
		Select Case Status
			Case "Normal":		 TextBoxTop.Text="": TextBoxBottom.Text=""
			Case "VoodooMagic":  TextBoxTop.Text="   * VOODOO *   ": TextBoxBottom.Text=""
			Case "Spitting": 	 TextBoxTop.Text="  * SPITTING *  "
			Case "HighDive": 	 TextBoxTop.Text=" * HIGH DIVE *  "
			Case "SwordMaster":  TextBoxTop.Text=" *SWORD MASTER* "
			Case "DaintyLady": 	 TextBoxTop.Text=" *DAINTY LADY*  "
			Case "MonkeyCombat": TextBoxTop.Text="*MONKEY COMBAT* "
			Case "TreasureHunt": TextBoxTop.Text="  * TREASURE *  "
			Case "GraveDigger":  TextBoxTop.Text=" *GRAVE DIGGER* "
			Case "GoEscape": 	 TextBoxTop.Text=" "&EscapeNb&" HITS TO GO"
		End Select
		ModeLengthTimer.Enabled=True
	End If
End Sub

Sub MoreTime()
	ModeLengthTimer.Enabled=False
	Select Case Status
	Case "DaintyLady"
		ModeTimeLeft=ModeTimeLeft+3
		MoreTimeDelay=2000
	Case "MonkeyCombat"
		ModeTimeLeft=ModeTimeLeft+10
		MoreTimeDelay=7000
	Case "TreasureHunt"
		ModeTimeLeft=ModeTimeLeft+6
		Exit Sub
	Case "GraveDigger"
		ModeTimeLeft=ModeTimeLeft+3
		MoreTimeDelay=2000
	End Select
	MoreTimeTimer.Interval=MoreTimeDelay
	MoreTimeTimer.Enabled=True
	DisplayQueueScreen "  EXTRA  TIME   ","    AWARDED     ",3,3,0,1000,True,"parrotcall"
End Sub

Sub MoreTimeTimer_Timer()
	MoreTimeTimer.Enabled=False
	ModeLengthTimer.Enabled=True
	MoreTimeDelay=0
End Sub

Sub ResetModeLights()
	LightArray(0, 1) = 2
	LightArray(5, 1) = 2
	LightArray(8, 1) = 2
	LightArray(9, 1) = 2
	LightArray(10, 1) = 2
	LightArray(11, 1) = 2
	LightArray(12, 1) = 2
	LightArray(6, 1) = 2
	LightArray(25, 1) = 2
End Sub

Sub CancelModeLights()
	If ModesGet(7) <> 1 Then LightArray(0, 1) = 0
	If ModesGet(8) <> 1 Then LightArray(5, 1) = 0
	If ModesGet(1) <> 1 Then LightArray(8, 1) = 0
	If ModesGet(2) <> 1 Then LightArray(9, 1) = 0
	If ModesGet(3) <> 1 Then LightArray(10, 1) = 0
	If ModesGet(4) <> 1 Then LightArray(11, 1) = 0
	If ModesGet(5) <> 1 Then LightArray(12, 1) = 0
	If ModesGet(6) <> 1 Then LightArray(6, 1) = 0
	If ModesGet(6) <> 1 Then LightArray(25, 1) = 0
End Sub

'***************************** high dive *************************************

Sub HighDive()
'repeat sequences of key presses: left, right, fire, start
'sequences get faster each stage. Needs 6 sequences
'lock ball in grog machine. Pause for instructions
'sequence displayed. Pause for message. Timer runs down
'keys are pressed. Timer finishes. Sequence compared
'results. Repeat?. Mode total
'each key press increases a variable (DiveKeyNumber)
'check if too many keys pressed (DiveKeyLimit)
'End If
'show display
	WaSpit.IsDropped = 1					'EP- Make sure the spitting wall is dropped
	LightArray(9, 1) = 1
	SetPulse 9, 1
	Status="HighDive"
	DiveNumber=0
	NextDiveUpper=""
	NextDiveLower=""
	TempDiveKey="                "
	DiveKeyNumber=0
	NextDSNumber=0
	DivesPerformed=0
	DisplayFlushQueue()
	DisplayQueueScreen "    HIGH DIVE   ","    HIGH DIVE   ",8,8,0,1500,False,"diveagainstyou"
	DisplayQueueScreen "   SHOOT  THE   ","  GROG MACHINE  ",0,0,0,1000,False,""
	DisplayQueueScreen "    TO ENTER    ","   COMPETITION  ",0,0,0,1500,True,""
	GrogKicker.Enabled=True
	LightArray(41, 1) = 2
	GiGrogTimer.Enabled = 1
	CalcDive()
	BallSave=1
	LightArray(22, 1) = 2
End Sub

Dim GrogMover
Sub GrogKicker_Hit()
'	GrogKicker.DestroyBall
	Set GrogMover = ActiveBall
	ModeLengthTimer.Enabled=False 'turn off the mode timer while doing things
	PlaySound "ballinr"
	PlaySound "intogrog"
	LightArray(41, 1) = 0
	GrogLight(1)
	GiGrogTimer.Enabled = 0
	Select Case Status
	Case "Normal": 		 GoGrog() 'collect barrels
	Case "Spitting": 	 GrogIntro()
	Case "HighDive": 	 DiveIntro()
	Case "TreasureHunt": If TH_ToDo=1 Then TreasureHunt_One
	End Select
End Sub

Sub CalcDive()
	Dim TempDive
	For x=1 To 9
		TempDive=round(rnd*3+1)
		Select Case TempDive
		Case 1: DS(x)="LEFT"
		Case 2: DS(x)="RIGHT"
		Case 3: DS(X)="FIRE"
		Case 4: DS(x)="START"
		End Select
	Next
End Sub

Sub DiveIntro()
	DiveIntroTimer.Enabled=True
	Dive=1							' we are in pure Dive sequence
	If Tilt = 1 Then Exit Sub		' Nothing to do here
	DiveNumber=DiveNumber+1
	DiveRangeStart=1
	DiveRangeEnd=DiveNumber+3
	DiveKeyLimit=DiveNumber+3
	Select Case DiveNumber
		Case 1: NextDiveUpper="     DOUBLE     ": NextDiveLower="   FLIP TWIST   "
		Case 2: NextDiveUpper="   TRIPLE SPIN  ": NextDiveLower="    BACKFLIP    "
		Case 3: NextDiveUpper="    FOREWARDS   ": NextDiveLower="    1080 ROLL   "
		Case 4: NextDiveUpper="   FRONT FLIP   ": NextDiveLower="  REVERSE TWIST "
		Case 5: NextDiveUpper="  TRIPLE FRONT  ": NextDiveLower="  ROLLING FLIP  "
		Case 6: NextDiveUpper="  BACKWARDS 720 ": NextDiveLower="  TWISTING FLIP "
	End Select
	DisplayFlushQueue()
	DisplayQueueScreen " COPY SEQUENCE  ","TO PERFORM DIVE ",3,3,0,1500,False,""
	DisplayQueueScreen "   NEXT  DIVE   ","       IS       ",0,0,0,1500,False,""
	DisplayQueueScreen NextDiveUpper,NextDiveLower,8,8,0,1500,False,""
	DisplayQueueScreen "      WATCH     ","      THIS      ",3,3,0,1500,False,""
'	GrogKicker.DestroyBall
	NextDiveNumber=DiveRangeStart-1
	CorrectDiveCount=0
	DiveKeyNumber=0
	If Play_Mode = "Hard" Then CalcDive
End Sub

Sub DiveIntroTimer_Timer()
	DiveIntroTimer.Enabled=False
	If Tilt = 1 Then Exit Sub		' Nothing to do here
	DiveTimer.Enabled=True			'show sequence
End Sub

Sub DiveTimer_Timer() 'show the dive sequence
	DiveTimer.Enabled=False
	If Tilt = 1 Then Exit Sub		' Nothing to do here
	Kicker7.Kick 180,4 'high dive out
	DisplayFlushQueue()
	NextDiveNumber=NextDiveNumber+1
	If NextDiveNumber=DiveRangeEnd-1 Then PlaySound "springboard"
	DisplayQueueScreen "     "&DS(NextDiveNumber),"                ",0,0,0,1500,False,"release"
	If DS(NextDiveNumber)="LEFT"  Then Light80.State = 1
	If DS(NextDiveNumber)="RIGHT" Then Light81.State = 1
	If DS(NextDiveNumber)="FIRE"  Then Light56.State = 1
	If DS(NextDiveNumber)="START" Then StartLight.State = 1
	CancelDiveKeyTimer.Enabled=True
	If NextDiveNumber=DiveRangeEnd Then
		TakeDive()
	Else
		DiveTimer.Enabled=True
	End If
End Sub

Sub TakeDive()
	If Tilt = 1 Then Exit Sub		' Nothing to do here
	PlaySound "gooddive"
	TakeDiveTimer.Enabled=True
	DisplayQueueScreen "  OKAY GUYBRUSH ","  ITS YOUR TURN ",3,3,0,1500,False,""
	DisplayQueueScreen "      READY     ","      READY     ",0,0,0,1000,False,""
	NextDSNumber=DiveRangeStart-1
	DiveKeyNumber=0
End Sub

Sub TakeDiveTimer_Timer()
	TakeDiveTimer.Enabled=False
	If Tilt = 1 Then Exit Sub		' Nothing to do here
	DiveActive=True
	DisplayFlushQueue()
End Sub

Sub DiveProcessKey(keycode)
	If DiveActive=False Then Exit Sub
	CancelDiveKeyTimer.Enabled=True
	Select Case (Keycode)
		Case LeftflipperKey:  TempDiveKey="LEFT":  Light80.State = 1
		Case RightflipperKey: TempDiveKey="RIGHT": Light81.State = 1
		Case PlungerKey: 	  TempDiveKey="FIRE":  Light56.State = 1
		Case 2: 			  TempDiveKey="START": StartLight.State = 1
	End Select
	CheckDive()
End Sub

Sub CancelDiveKeyTimer_Timer() 'clear lights and display
	CancelDiveKeyTimer.Enabled=False
	Light56.State = 0					'Fire light
	StartLight.State = 0				'Start light
	Light80.State = 0					'Left Flipper
	Light81.State = 0					'Righ Flipper
	TextBoxTop.Text = "                ": TextBoxBottom.Text= "                "
End Sub

Sub CheckDive() 'displayes your dive keypresses
	NextDSNumber=NextDSNumber+1
	DiveKeyNumber=DiveKeyNumber+1
	TempDS(NextDSNumber)=TempDiveKey
	If Tilt <> 1 Then
		TextBoxTop.Text = "     "&TempDiveKey&"      "
		TextBoxBottom.Text="                "
		If TempDS(NextDSNumber)=DS(NextDSNumber) Then
			PlaySound "release"
			CorrectDiveCount=CorrectDiveCount+1
		Else
			PlaySound "doh"
		End If
	Else
		PlaySound "doh"
	End If
	If DiveKeyNumber=DiveKeyLimit Then ShowDive()
End Sub

Sub ShowDive()
	DiveActive=False
	If Tilt = 1 Then Exit Sub		' Nothing to do here
	DiveWaitTimer.Enabled=True
	NextDSNumber=DiveRangeStart-1
End Sub

Dim DiveMover
Sub Kicker4_Hit() 'bottom trap
	Set DiveMover = ActiveBall
	Me.Timerenabled = 1
End Sub

Sub Kicker4_Timer()
	Me.TimerEnabled = 0
	DiveMover.X = Kicker10.X
	DiveMover.Y = Kicker10.Y
	DiveMover.Z = 350
	Me.Kick 0, 0, 0
	'EP- Flash a big light in front of the monkey
End Sub

Sub DiveWaitTimer_Timer()
	DiveWaitTimer.Enabled=False
	If Tilt = 1 Then Exit Sub		' Nothing to do here
	DisplayFlushQueue()
	DisplayQueueScreen "   LETS WATCH   ","   YOUR DIVE    ",3,3,0,1500,False,""
	Kicker10.Kick 3,35
	PlaySound "ballroll": PlaySound "ballinr"
	YourDiveWaitTimer.Enabled=True
End Sub

Sub YourDiveWaitTimer_Timer() 'pause to show the above display
	YourDiveWaitTimer.Enabled=False
	If Tilt = 1 Then Exit Sub		' Nothing to do here
	TestDiveTimer.Enabled=True 'show the moves
End Sub

Sub Kicker7_Hit()
	Set DiveMover = ActiveBall
	Select Case Status
	Case "Normal"
	Case "Spitting"
		If Tilt=1 Then Exit Sub
		TestSpitTimer.Enabled=True 'ball resetting only
		DiveMover.X = Kicker10.X
		DiveMover.Y = Kicker10.Y
		DiveMover.Z = 350
		Me.Kick 0, 0, 0
		GoSpitAward()
	Case "HighDive"	'dont use the timer with this one as kicks after display for dive
	End Select
End Sub

Sub TestDiveTimer_Timer()
	TestDiveTimer.Enabled=False 	'show your next move
	If Tilt = 1 Then Exit Sub		' Nothing to do here
	DisplayFlushQueue()
	NextDSNumber=NextDSNumber+1
	TextBoxTop.Text = "     "&TempDS(NextDSNumber)&"      "
	TextBoxBottom.Text= "                "
	If TempDS(NextDSNumber)="LEFT"  Then Light80.State = 1
	If TempDS(NextDSNumber)="RIGHT" Then Light81.State = 1
	If TempDS(NextDSNumber)="FIRE"  Then Light56.State = 1
	If TempDS(NextDSNumber)="START" Then StartLight.State = 1
	CancelDiveKeyTimer.Enabled=True
	If NextDSNumber=DiveRangeEnd Then
		TestDisplayTimer.Enabled=True
	Else
		TestDiveTimer.Enabled=True
	End If
End Sub

Sub TestDisplayTimer_Timer()
	TestDisplayTimer.Enabled=False
	TestDive()
End Sub

Sub TestDive()
	If Tilt = 1 Then Exit Sub		' Nothing to do here
	Kicker7.Kick 180,5
	PlaySound "springboard"
	JudgeScore(1)=0: JudgeScore(2)=0: JudgeScore(3)=0
	If CorrectDiveCount=DiveNumber+3 Then
		If CorrectDiveCount=9 Then
			JudgeScore(1)=10: JudgeScore(2)=10: JudgeScore(3)=10: GoCorrectDive
		Else
			While (JudgeScore(1)+JudgeScore(2)+JudgeScore(3))<ScoreMini(DiveNumber) Or (JudgeScore(1)+JudgeScore(2)+JudgeScore(3))>ScoreMaxi(DiveNumber)
				JudgeScore(1)=round(rnd*CoeffDive(DiveNumber))
				JudgeScore(2)=round(rnd*CoeffDive(DiveNumber))
				JudgeScore(3)=round(rnd*CoeffDive(DiveNumber))
			Wend
			If DiveToDo*CorrectDiveCount=28 Then JudgeScore(1)=8: JudgeScore(2)=8: JudgeScore(3)=8
		GoCorrectDive()
		End If
	Else
		GoWrongDive()
	End If
End Sub

Sub JudgeTimer_Timer()
	JudgeTimer.Enabled=False
	If Tilt = 1 Then Exit Sub		' Nothing to do here
	ModeTimeLeft=ModeTimeLeft+8-DivesPerformed
	If ModeTimeLeft<5 Then ModeTimeLeft=5
	GrogMover.X = Kicker11.X
	GrogMover.Y = Kicker11.Y
	GrogMover.Z = 370
	Set GrogMover = Nothing
	'EP- Do some lighting FX to inidicate the ball is about to come out of LeChuck
	Kicker11.TimerEnabled=True
	PlaySound "ballout": PlaySound "ballroll2"
End Sub

Sub GoCorrectDive()
	If Tilt = 1 Then Exit Sub		' Nothing to do here
	PlaySound "gooddive"
	DivesPerformed=DivesPerformed+1
	JudgeTimer.Enabled=True
	DisplayFlushQueue()
	DisplayQueueScreen "    NICE DIVE   ","    GUYBRUSH    ",8,8,0,1500,False,""
	DisplayQueueScreen "    JUDGES =    ","                ",2,1,0,1000,False,""
	If DiveNumber < 6 Then
		DisplayQueueScreen "[1]   [2]   [3] "," "&JudgeScore(1)&"     ",0,0,0,1500,False,""
		DisplayQueueScreen "[1]   [2]   [3] "," "&JudgeScore(1)&"     "&JudgeScore(2)&"     ",0,0,0,1500,False,""
		DisplayQueueScreen "[1]   [2]   [3] "," "&JudgeScore(1)&"     "&JudgeScore(2)&"     "&JudgeScore(3),0,0,0,1500,False,""
	Else
		DisplayQueueScreen "[1]   [2]   [3] ","10   ",0,0,0,1500,False,""
		DisplayQueueScreen "[1]   [2]   [3] ","10    10  ",0,0,0,1500,False,""
		DisplayQueueScreen "[1]   [2]   [3] ","10    10    10 ",0,0,0,1500,False,""
	End If
	ModeTotal=ModeTotal+((JudgeScore(1) + JudgeScore(2) + JudgeScore(3))*200000)
	If Play_Mode="Medium" And DiveNumber>1 Then ModeTotal=ModeTotal - ((5+DiveNumber)*100000)
	DisplayQueueScreen "    PRIZE =     ","  "&FormatScore(ModeTotal)&"  ",0,8,0,1500,False,"nicedive"
	If DiveNumber=DiveToDo Then
		DisplayQueueScreen "   HIGH  DIVE   ","    CHAMPION    ",8,8,0,1500,False,"newchampion"
		DisplayQueueScreen "     HURRY      "," SHOOT LE CHUCK ",3,3,0,1500,False,"init5"
' Dive HurryUpWaitTimer must be greater to JudgeTimer (13500)
		HurryUpWaitTimer.Interval=18000
		HurryUpWaitTimer.Enabled=True
		GrogLight(-1)
		GiGrogTimer.Enabled = 0
		GrogKicker.Enabled=False
		LightArray(49, 1) = 1
		LightArray(31, 1) = 2
	Else
		DisplayQueueScreen "     GO FOR     ","  THE NEXT ONE  ",0,0,0,1500,False,""
		DisplayQueueScreen "   SHOOT  THE   ","  GROG MACHINE  ",3,3,0,1500,False,""
		GiGrogtimer.Enabled = 1
		DisplayScoreQueue 2,1
	End If
End Sub

Sub PFMultiplierLight_Timer()
	DisplayFlushQueue()
	DisplayQueueScreen "  PLAYFIELD X3  ","    IS OVER     ",3,3,0,1000,False,"init5"
	PFMultiplierLight.TimerEnabled=False: PFMultiplierLight.State=LightStateOff
	PFMultiplier=1
End Sub

Sub GoWrongDive()
	If Tilt = 1 Then Exit Sub		' Nothing to do here
	DisplayFlushQueue()
	DisplayQueueScreen "    BAD LUCK    ","    GUYBRUSH    ",3,3,0,1500,False,"baddive"
	DisplayQueueScreen "     "&DivesPerformed&"  DIVES  ","  PRIZE = "&(DivesPerformed*10000),0,3,0,1500,False,""
	DisplayScoreQueue 2,1
	ModeTotal=ModeTotal+(DivesPerformed*10000)
	GoWrongTimer.Enabled=True
End Sub

Sub GoWrongTimer_Timer()
	GoWrongTimer.Enabled=False
	GrogLight(-1)
	GiGrogTimer.Enabled = 0
	GrogMover.X = Kicker11.X
	GrogMover.Y = Kicker11.Y
	GrogMover.Z = 370
	Set GrogMover = Nothing
	PlaySound "ballroll2": PlaySound "ballout"
	Kicker11.TimerEnabled=True
	GoModeTotal()
End Sub

'***************************** spitting competition *************************************

Sub SpittingCompetition()
	WaSpit.IsDropped = 0
	'lock ball in grog machine
	'press flippers to keep up power,
	'accumulator keeps track of how many presses
	'accumulator reduces on a timer
	LightArray(10, 1) = 1
	SetPulse 10, 1
	Status="Spitting"
	DisplayFlushQueue()
	DisplayQueueScreen "    SPITTING    ","   COMPETITION  ",3,3,0,1500,False,"spitting"
	DisplayQueueScreen "   SHOOT  THE   ","  GROG MACHINE  ",0,0,0,1000,False,""
	DisplayQueueScreen "    TO ENTER    "," THE CHALLENGE  ",0,0,0,1500,True,""
	PlaySound "spit"
	GrogKicker.Enabled=True
	LightArray(41, 1) = 2
	GiGrogTimer.Enabled = 1
	NextSpitDistance=SpittingDistance(1)
	SpitNumber=1
	SpittingDistance(5)=999
	BallSave=1
	LightArray(22, 1) = 2
End Sub

Sub GrogIntro()
	Spitting=1		' we are in pure Spitting sequence
	SpitPower=1
	SpitPic=1
	SpitCount=0
	DisplayFlushQueue()
	DisplayQueueScreen "  ALT FLIPPERS  "," FOR SPIT POWER ",3,3,0,2000,False,""
	DisplayQueueScreen "  TARGET IS =   ","AT LEAST "&NextSpitDistance&" FT ",0,0,0,1000,False,""
	DisplayQueueScreen "     READY      ","     READY      ",2,1,0,1000,False,""
	DisplayQueueScreen "     STEADY     ","     STEADY     ",0,0,0,1000,False,""
	DisplayQueueScreen "       GO       ","       GO       ",8,8,0,500,False,""
	If NextSpitDistance=SpittingDistance(1) Then SpitTimer.Interval=160+SpitAdjust
	If NextSpitDistance=SpittingDistance(2) Then SpitTimer.Interval=150+SpitAdjust
	If NextSpitDistance=SpittingDistance(3) Then SpitTimer.Interval=140+SpitAdjust
	If NextSpitDistance=SpittingDistance(4) Then SpitTimer.Interval=140+SpitAdjust
	GrogKicker.TimerEnabled=True 'wait before starting comp
End Sub

Sub GrogKicker_Timer()	'start comp
	GrogKicker.TimerEnabled=False
	Select Case Status
	Case "Normal"
		GrogKicker.Kick 4,22
	Case "Spitting"
		If Tilt=1 Then Exit Sub
		'Kicker10.CreateBall.Image="green ball"				'EP- Can I change the image of the current ball?
		PlaySound "ballinr": PlaySound "rattle"
		DisplayFlushQueue()
		SpitActive=True 'allows spit flipper operation only during timer routine
		SpitTimer.Enabled=True 'accumulator loop
	Case "TreasureHunt"
		GrogKicker.Kick 4,22
		PlaySound "ballinr"
		GrogKicker.Enabled=False
		ModeLengthTimer.Enabled=True
	End Select
End Sub

Sub SpitProcessKey(keycode)
	If Tilt=1 Then Exit Sub
	Select Case (Keycode)
	Case LeftflipperKey
		If SpitActive=True Then
			If NextSpitKey=0 Then 'left
				SpitPower=SpitPower+1
				PlaySound "urk"
			End If
			NextSpitKey=1 'right
		End If
	Case RightflipperKey
		If SpitActive=True Then
			If NextSpitKey=1 Then 'right
				SpitPower=SpitPower+1
				PlaySound "urk"
			End If
		NextSpitKey=0 'left
	End If
	End Select
End Sub

Sub SpitTimer_Timer()
	SpitTimer.Enabled=False
	If Tilt=1 Then Exit Sub
	SpitPic=Abs(Int(SpitPower/10))
	If SpitPic < 1  Then SpitPic=1
	If SpitPic > 13 Then SpitPic=13
	TextBoxTop.Text=" POWER = "&SpitPower: TextBoxBottom.Text=""&SpitPos(SpitPic)
	SpitPower=SpitPower-1
	If SpitPower < 1   Then SpitPower=1
	If SpitPower > 130 Then SpitPower=130
	PlaySound "clack"
	SpitCount=SpitCount+1
	If SpitCount=30 Then
		SpitActive=False
		FinishSpittingTimer.Enabled=True
		TextBoxTop.Text="   READY FOR"
		TextBoxBottom.Text="    " & SpitPower & " FEET"
		If SpitPic > 12 Then SpitPic=12
	Else
		SpitTimer.Enabled=True 'restart the timer
	End If
End Sub

Sub FinishSpittingTimer_Timer()
	FinishSpittingTimer.Enabled=False
	FinishSpit()
End Sub

Sub FinishSpit()
	PlaySound "spit"
	SpitTimer.Enabled=False
	SpitActive=False
	If Tilt=1 Then Exit Sub
	Kicker10.Kick 13,(SpitPic+3) * 2 'grog kickout
	PlaySound "release"
	PlaySound "ballroll"
	DisplayFlushQueue()
	DisplayQueueScreen "  * PHTOOEY *   ","       []       ",3,0,0,20,False,""
	DisplayQueueScreen "-","      [  ]      ",0,0,0,20,False,""
	DisplayQueueScreen "-","     [    ]     ",0,0,0,20,False,""
	DisplayQueueScreen "-","    [      ]    ",0,0,0,20,False,""
	DisplayQueueScreen "-","   [        ]   ",0,0,0,20,False,""
	DisplayQueueScreen "-","  [          ]  ",0,0,0,20,False,""
	DisplayQueueScreen "-"," [            ] ",0,0,0,20,False,""
	DisplayQueueScreen "-","       *        ",0,0,0,2000,False,"tin can"
End Sub

Sub Kicker10_Hit() 'top of grog machine - check spit distance
	If Tilt=1 Then Exit Sub
	If Status = "Spitting" Then
		PlaySound "ballinr"
		TestSpitTimer.Enabled=True
		TestSpit()
	End If
End Sub

Sub TestSpit()
	If Tilt=1 Then Exit Sub
	DisplayFlushQueue()
	DisplayQueueScreen "   LETS   SEE   ","  HOW  YA  DID  ",3,3,0,750,False,""
	ModeTotal=(ModeTotal+SpitPower*SpitCoeff)
	If SpitPower >= NextSpitDistance  Then
		GoSpitAward()
	Else
		DisplayQueueScreen "   NICE   TRY   ","    GUYBRUSH    ",3,3,0,750,False,""
		GoModeTotal()
		GrogLight(-1)
	End If
End Sub

Sub TestSpitTimer_Timer()
	TestSpitTimer.Enabled=False
	GrogLight(-1)
	GiGrogTimer.Enabled = 0
	GrogMover.X = Kicker11.X
	GrogMover.Y = Kicker11.Y
	GrogMover.Z = 370
	Set GrogMover = Nothing
	PlaySound "ballroll2": PlaySound "ballout"
	Kicker11.TimerEnabled=True
	'EP- Some LeChuck kickout FX
	Kicker11.TimerEnabled=True
	PlaySound "ballout": PlaySound "ballroll2"
	DisplayScoreQueue 2,1
End Sub

Sub GoSpitAward()
'	DisplayFlushQueue()							'EP- removed because it clears the "Let's see" stuff
	If Tilt=1 Then Exit Sub
	PlaySound "congratulations"
	SpitNumber=SpitNumber+1
	NextSpitDistance=SpittingDistance(SpitNumber)
	If SpitNumber=5 Then
		DisplayQueueScreen "  COMPETITION   ","     WINNER     ",8,8,0,2000,False,"iwon"
		DisplayQueueScreen "     HURRY      "," SHOOT LE CHUCK ",3,3,0,1500,False,"init5"
		ModeLengthTimer.Enabled=False
		GrogKicker.Enabled=False
		LightArray(49, 1) = 3
		GrogLight(-1)
		HurryUpWaitTimer.Interval=8000
		HurryUpWaitTimer.Enabled=True
		If Play_Mode="Easy" Then: 	If ModeTotal> 10000000 Then: Modetotal=10000000: End If
		If Play_Mode="Medium" Then: If ModeTotal> 15000000 Then: Modetotal=15000000: End If
		If Play_Mode="Hard" Then: 	If ModeTotal> 20000000 Then: Modetotal=20000000: End If
	Else
		DisplayQueueScreen "     TARGET     ","    REACHED     ",8,8,0,2000,False,"bellring"
		DisplayQueueScreen "   NOW GO FOR   ","    "&NextSpitDistance&"  FEET",3,3,0,2000,True,""
		LightArray(41, 1) = 2
		GiGrogTimer.Enabled = 1
		ModeTimeLeft=ModeTimeLeft+14-(2*SpitNumber)
	End If
End Sub

'**************************************** SwordMaster ***********************************************

Sub SwordMaster()
	LightArray(12, 1) = 1
	SetPulse 12, 1
	DisplayFlushQueue()
	DisplayQueueScreen "     SWORD      ","     MASTER     ",2,1,0,500,False,"another shot"
	DisplayQueueScreen "     SWORD      ","     MASTER     ",8,8,0,1000,False,""
	DisplayQueueScreen "   HIT INSULT   ","     TARGETS    ",0,0,0,1000,False,""
	DisplayQueueScreen "  TO BEAT THE   ","  SWORDMASTER   ",0,0,0,1500,True,""
	SwordComplete=0
	BallSave=1
	LightArray(22, 1) = 2
	Status="SwordMaster"
	SwordTimer.Enabled=True
	InsultOn()
	SetPulse 56, 1
	SetPulse 57, 1
	SetPulse 58, 1
	SetPulse 59, 1
	SetPulse 60, 1
	SetPulse 61, 1
End Sub

Sub SwordTimer_Timer() 'delay for intro
	SwordTimer.Enabled=False
	NextSword()
End Sub

Sub NextSword()
	InsultOff()
	NextSwordTimer.Enabled=True
	InsultOn()
End Sub

Sub NextSwordTimer_Timer()
	NextSwordTimer.Enabled=False
	SwordNumber=SwordNumber+1 '1-4
	DisplayFlushQueue()
	Select Case SwordNumber
	Case 1
		DisplayQueueScreen "    TODAY BY    ","     MYSELF     ",0,0,0,1250,False,"12people"
		DisplayQueueScreen "    12 PEOPLE   ","   IVE BEATEN   ",0,0,0,1250,True,""
	Case 2
		DisplayQueueScreen "    IM GOING    ","     TO PUT     ",0,0,0,1250,False,"arminasling"
		DisplayQueueScreen "    YOUR ARM    ","   IN A SLING   ",0,0,0,1250,True,""
	Case 3
		DisplayQueueScreen "   ONLY ONCE    ","   HAVE I MET   ",0,0,0,1250,False,"metacoward"
		DisplayQueueScreen "     SUCH A     ","     COWARD     ",0,0,0,1250,True,""
	Case 4
		DisplayQueueScreen "     IVE GOT    ","   MUSCLES IN   ",0,0,0,1250,False,"neverheardof"
		DisplayQueueScreen "   PLACES YOU   "," NEVER HEARD OF ",0,0,0,1250,True,""
	Case 5
		DisplayQueueScreen "   THATS  NOT   ","   AN  INSULT   ",3,3,0,1500,False,"thatsnotinsult"
		DisplayQueueScreen "   SWORDMASTER  ","    DEFEATED    ",8,8,0,1000,False,"init5"
		SwordNumber=SwordNumber-1
	End Select
	DisplayScoreQueue 1,2
End Sub

Sub CheckSword()
	FlashSword()
	PlaySound "punchgrog"
	PlaySound "clang"
	ModeTotal=ModeTotal+250000
	If (II="I") And (NN="N") And (SS="S") And (UU="U") And (LL="L") And (TT="T") Then
'all targets down
		If SwordNumber=SwordToDo Then SwordComplete=1
		InitSMTimer()
		DisplayFlushQueue()
		Select Case SwordNumber
		Case 1
			DisplayQueueScreen "  BY THE SIZE   ","  OF YOUR GUT   ",8,8,0,850,False,"guesseaten"
			DisplayQueueScreen " ID GUESS THEY  ","   WERE EATEN   ",8,8,0,850,True,""
			ModeTotal=ModeTotal+3000000: ModeTimeLeft=25+Bananas
		Case 2
			DisplayQueueScreen "  WHY ARE YOU   ","    STUDYING    ",8,8,0,850,False,"beanurse"
			DisplayQueueScreen "     TO BE      ","     A NURSE    ",8,8,0,850,True,""
			ModeTotal=ModeTotal+4000000: ModeTimeLeft=20+Bananas
		Case 3
			DisplayQueueScreen "  HE MUST HAVE  ","   TAUGHT YOU   ",8,8,0,850,False,"everythingyouknow"
			DisplayQueueScreen "   EVERYTHING   ","    YOU KNOW    ",8,8,0,850,True,""
			ModeTotal=ModeTotal+4000000: ModeTimeLeft=15+Bananas
		Case 4
			DisplayQueueScreen "  TOO BAD NONE  ","     OF THEM    ",8,8,0,850,False,"noneinarms"
			DisplayQueueScreen "     ARE IN     ","    YOUR ARMS   ",8,8,0,850,True,""
			ModeTotal=ModeTotal+4000000
		Case 5
			DisplayQueueScreen "     HURRY      "," SHOOT LE CHUCK ",3,3,0,1500,True,"init5"
			SwordNumber=5 'make this loop until lechuck hit or mode over
		End Select
		CheckSwordTimer.Enabled=True
	Else
		DisplayScoreQueue 1,2
	End If
End Sub

Sub CheckSwordTimer_Timer()
	CheckSwordTimer.Enabled=False
	If SwordNumber=SwordToDo Then
		Playsound "iwon"
		DisplayQueueScreen "     SHOOT      ","     LECHUCK    ",3,3,0,1000,True,"init5"
		LightArray(31, 1) = 3
		ModeLengthTimer.Enabled=False
		LightArray(49, 1) = 1
	End If
	If SwordNumber < SwordToDo Then NextSword()
End Sub

Dim SwordX, SwordY
Sub InitSMTimer()
	LightArray(24, 1) = 1
	ModeLengthTimer.Enabled=False
	BallSM.VelX=0: BallSM.VelY=0
	If SwordComplete=1 Then
		Kicker9.TimerInterval=5500: Magnet2Kicker.TimerInterval=5500
	End If
	Kicker9.Timerenabled = 1
	SMTimer.Enabled = 1
	SwordX = BallSM.X
	SwordY = BallSM.Y
End Sub

Sub Kicker9_Timer()
	Kicker9.TimerEnabled=False
	If Tilt=1 Then Exit Sub
	If Status="TreasureHunt" Then ModeLengthTimer.Enabled=True
	If Status="SwordMaster"  Then
		If SwordComplete=1 Then
			SMTimer.Enabled = 0
			HurryUpWaitTimer.Interval=100
			HurryUpWaitTimer.Enabled=True
		Else
			SMTimer.Enabled = 0
			ModeLengthTimer.Enabled=True
		End If
	End If
	Kicker9.TimerInterval=9000
End Sub

Sub SMTimer_Timer()
	BallSM.X = SwordX
	BallSM.Y = SwordY
	BallSM.VelX = 0
	BallSM.VelY = 0
End Sub

'********************************* dainty lady **************************************

Sub DaintyLady()
	ShipTimer.Enabled=True 'next target
	InsultOff()
	CenterArrowsOff()
	LightArray(11, 1) = 1
	SetPulse 11, 1
	DisplayFlushQueue()
	DisplayQueueScreen "     DAINTY     ","      LADY      ",2,1,0,500,False,"pirateship"
	DisplayQueueScreen "     DAINTY     ","      LADY      ",8,8,0,1000,False,""
	DisplayQueueScreen "  HIT FLASHING  ","    LIGHT TO    ",0,0,0,1000,False,""
	DisplayQueueScreen "    GET THE     ","      SHIP      ",0,0,0,1500,True,"lechucksproperty"
	BallSave=1
	LightArray(22, 1) = 2
	Status="DaintyLady"
End Sub

Sub ShipTimer_Timer()
	ShipTimer.Enabled=False
	DisplayFlushQueue()
	ResetShipTargets()
	If ShipNumber=1 Then DisplayQueueScreen "  FIND YOURSELF ","     A CREW     ",3,3,0,1000,False,""
	If ShipNumber=2 Then DisplayQueueScreen "     GRAB A     ","    NAVIGATOR   ",3,3,0,1000,False,""
	If ShipNumber=3 Then DisplayQueueScreen "   GO AND GET   ","    YOUR SHIP   ",3,3,0,1000,False,""
	LastShipTarget=ShipTarget
	While ShipTarget=LastShipTarget 'should stop the last target from being selected
		ShipTarget=round(rnd*5)
	Wend
	If ShipNumber=1 Then
		If ShipTarget=2 Then '1st shot - dont kick out ball into it!!!
			ShipTarget=3
		End If
	End If
	If ShipTarget=1 Then LightArray(33, 1) = 1
	If ShipTarget=2 Then LightArray(33, 1) = 1
	If ShipTarget=3 Then LightArray(42, 1) = 1
	If ShipTarget=4 Then LightArray(28, 1) = 1
	If ShipTarget=5 Then LightArray(41, 1) = 1
	LastShipTarget=ShipTarget
	ShipMoveTimer.Enabled=True 'start the countdown to next shot
	DisplayScoreQueue 0,0
End Sub

Sub ShipMoveTimer_Timer() '5 seconds up -move the target!
	ShipMoveTimer.Enabled=False
	DisplayFlushQueue()
	MoveShip()
End Sub

Sub MoveShip()
	PlaySound "splash44"
	DisplayQueueScreen "     TARGET     ","      MOVES     ",3,3,0,1000,False,""
	ResetShipTargets()
	While ShipTarget=LastShipTarget 'should stop the last target from being selected
		ShipTarget=round(rnd*4+1)
	Wend
	If ShipTarget=1 Then LightArray(33, 1) = 1
	If ShipTarget=2 Then LightArray(33, 1) = 1
	If ShipTarget=3 Then LightArray(42, 1) = 1
	If ShipTarget=4 Then LightArray(28, 1) = 1
	If ShipTarget=5 Then LightArray(41, 1) = 1
	LastShipTarget=ShipTarget
	ShipMoveTimer.Enabled=True 'start the countdown again
	If ShipNumber=1 Then DisplayQueueScreen "  FIND YOURSELF ","     A CREW     ",3,3,0,1000,False,""
	If ShipNumber=2 Then DisplayQueueScreen "     GRAB A     ","    NAVIGATOR   ",3,3,0,1000,False,""
	If ShipNumber=3 Then DisplayQueueScreen "   GO AND GET   ","    YOUR SHIP   ",3,3,0,1000,False,""
	DisplayScoreQueue 0,0
End Sub

Sub GotShipTarget()
	DisplayFlushQueue()
	ShipMoveTimer.Enabled=False
	If ShipNumber=1 Then
		PlaySound "gotacrew"
		DisplayQueueScreen "      CREW      ","      HIRED     ",8,8,0,600,False,""
		DisplayQueueScreen "       " & DaintyLadyScores(1) & "  ","    MILLIONS    ",0,0,0,1000,False,""
		ModeTotal=ModeTotal+(DaintyLadyScores(1)*1000000)
		MoreTime()
		MoveShip()
	End If
	If ShipNumber=2 Then
		PlaySound "gotanavigator"
		DisplayQueueScreen "    NAVIGATOR   ","      HIRED     ",8,8,0,600,False,""
		DisplayQueueScreen "       " & DaintyLadyScores(2) & "  ","    MILLIONS    ",0,0,0,1000,False,""
		ModeTotal=ModeTotal+(DaintyLadyScores(2)*1000000)
		MoreTime()
		MoveShip()
	End If
	If ShipNumber=3 Then
		ShipTimer.Enabled=False 'stop the main mode timer and start hurryup timer
		PlaySound "showyoutoship"
		DisplayQueueScreen "      SHIP      ","    ACQUIRED   ",8,8,0,400,False,""
		DisplayQueueScreen "       " & DaintyLadyScores(3) & "  ","    MILLIONS   ",0,0,0,750,False,""
		DisplayQueueScreen "     SHOOT      ","    LE CHUCK    ",3,3,0,1500,False,"init5"
		ModeLengthTimer.Enabled=False 'stops main timer
		ModeTotal=ModeTotal+(DaintyLadyScores(3)*1000000)
		HurryUpWaitTimer.Enabled=True
	End If
	ShipNumber=ShipNumber+1 'shot number (so check 4 when hitting lechuck!!!)
End Sub

Sub ResetShipTargets()
	LightArray(33, 1) = 0
	LightArray(33, 1) = 0
	LightArray(42, 1) = 0
	LightArray(28, 1) = 0
	LightArray(41, 1) = 0
End Sub

' ******************************* Mad Monkey Combat **********************************************

Sub MadMonkeyCombat()
'Hit the monkey targets in order,
'Complete 3 sets to defeat lechuck.
'Gives bronze hat on completion if lechuck hit.
'variables:
'MadSetNumber -number of set
'MadNumber -which mad target number we are on
'MadTargetNumber -which target we have hit
'MadTarget(1), (2), (3) -values for each target
'WhatMadTarget -how many targets have been hit in sequence
'CorrectMadHits -how many times correct targets hit
'CorrectMadSet -number of sets complete
	DisplayFlushQueue
	DisplayQueueScreen "    ULTIMATE    ","    ULTIMATE    ",0,0,0,1500,False,"bigstep"
	DisplayQueueScreen "     MONKEY     ","     MONKEY     ",0,0,0,1500,False,"bigstep"
	DisplayQueueScreen "     COMBAT     ","     COMBAT     ",8,8,0,1500,False,"bigstep"
	DisplayQueueScreen "  HIT MONKEYS   ","  IN SEQUENCE   ",0,0,0,1000,False,""
	DisplayQueueScreen "   "&MadCombatToDo&" ROUNDS     ","   TO DEFEAT   ",0,0,0,1000,False,""
	DisplayQueueScreen "      MEGA      ","    LECHUCK     ",0,0,0,1000,False,""
	DisplayQueueScreen "      MEGA      ","    LECHUCK     ",3,3,0,500,False,"ichallenge"
	FirstMadTimer.Enabled=True
	Status="MonkeyCombat"
	DisplayScoreQueue 0,0
	BallSave=1: ShootAgainLight.State=LightStateBlinking
	NewMadTargetTimer.Interval=9000: NewMadTargetTimer.Enabled=True
	CombatLight.State=LightStateOn
	MadAllowed=0 'not yet!
	ResetMonkeys
End Sub

Sub FirstMadTimer_Timer()
	FirstMadTimer.Enabled=False
	PlaySound "ballinr": PlaySound "doorclose"
	BallSaveTimer.Enabled=False
	BallSaveTimer.Enabled=True
End Sub

Sub NewMadTargetTimer_Timer() 'delay for each target set
	NewMadTargetTimer.Enabled=False
	If Tilt = 1 Then Exit Sub
	NewMadTargetTimer.Interval=5000
	DisplayFlushQueue
	DisplayQueueScreen "    LE CHUCK    ","     MOVES     ",3,3,0,500,False,"bigstep"
	eekLight.State=LightStateOff: ackLight.State=LightStateOff: ookLight.State=LightStateOff
	MadTargetTimer.Enabled=True 'starts each individual target timer
	MadTarget(1)=round(rnd*2+1)
End Sub

Sub ResetMadStuff()
	eekLight.State=LightStateOff: ackLight.State=LightStateOff: ookLight.State=LightStateOff
	MadTargetNumber=99
	WhatMadTarget=0: MadNumber=0
	MadTarget(1)=0: MadTarget(2)=0: MadTarget(3)=0
	CorrectMadHits=0
	NewMadTargetTimer.Enabled=True 'start the set again
End Sub

Sub MadTargetTimer_Timer() 'individual target timers
	MadTargetTimer.Enabled=False
	If Tilt = 1 Then Exit Sub
	NewMadTarget
End Sub

Sub NewMadTarget()
	MadNumber=MadNumber+1
	If MadNumber=1 Then FirstMadTarget
	If MadNumber=2 Then SecondMadTarget
	If MadNumber=3 Then ThirdMadTarget
End Sub

Sub FirstMadTarget()
	SequenceToDo="HIT"
	DisplayMadTarget
	MadTargetTimer.Enabled=True 'turn on timer for second target
End Sub

Sub SecondMadTarget()
	MadTarget(2)=Madtarget(1)
	While MadTarget(2)=Madtarget(1): MadTarget(2)=round(rnd*2+1): Wend
	DisplayMadTarget
	MadTargetTimer.Enabled=True 'turn on timer for third target
End Sub

Sub ThirdMadTarget()
	MadTarget(3)=1
	While MadTarget(3)=Madtarget(1) Or MadTarget(3)=Madtarget(2): MadTarget(3)=round(rnd*2+1): Wend
	DisplayMadTarget
	MadAllowed=1 		'can now hit monkey targets
	ModeTimeLeft=ModeTimeLeft-3: If ModeTimeLeft<3 Then ModeTimeLeft=3
	ModeLengthTimer.Enabled=True
	DisplayScoreQueue 0,0
End Sub

Sub ShowLastMoveTimer_Timer() 'time this to sync with music
	ShowLastMoveTimer.Enabled=False
	LastMoveTimer.Enabled=True
End Sub

Sub LastMoveTimer_Timer()
	LastMoveTimer.Enabled=False
	MadNumber=MadNumber+1
	If MadNumber<4 Then LastMoveTimer.Enabled=True
	DisplayMadTarget
End Sub

Sub DisplayMadTarget()
	If MadTarget(MadNumber)=1 Then
		eekLight.State=LightStateBlinking
		PlaySound "bigchuckeek"
		DisplayFlushQueue
		DisplayQueueScreen "      EEK       ","      EEK      ",0,0,0,500,False,""
		SequenceToDo=SequenceToDo & " EEK"
		EekVel = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
		LUpTimer.Enabled=True
		PlaySound "monkeytargup"
	End If
	If MadTarget(MadNumber)=2 Then
		ackLight.State=LightStateBlinking
		PlaySound "bigchuckack"
		DisplayFlushQueue
		DisplayQueueScreen "      ACK       ","      ACK      ",0,0,0,500,False,""
		SequenceToDo=SequenceToDo & " ACK"
		AckVel = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
		MUpTimer.Enabled=True
	End If
	If MadTarget(MadNumber)=3 Then
		ookLight.State=LightStateBlinking
		PlaySound "bigchuckoop"
		DisplayFlushQueue
		DisplayQueueScreen "      OOK       ","      OOK      ",0,0,0,500,False,""
		SequenceToDo=SequenceToDo & " OOK"
		OokVel = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
		RUpTimer.Enabled=True
	End If
	If MadNumber=3 Then
		MadNumber=0
		LastMoveTimer.Enabled=False
		ShowLastMoveTimer.Enabled=True 'timer to keep repeating moves in time with song : )
	End If

End Sub

'*** hitting the targets

Sub MadMiss()
	DisplayQueueScreen "      MISS      ","      MISS     ",3,3,0,1500,True,"doh"
	CorrectMadHits=0
	WhatMadTarget=0
	AKick.Enabled=False: EKick.Enabled=False: OKick.Enabled=False
	BounceLTrig.Enabled=True: BounceMTrig.Enabled=True: BounceRTrig.Enabled=True
	LastMoveTimer.Enabled=True 'show the last move
	ShowLastMoveTimer.Enabled=False: ShowLastMoveTimer.Enabled=True 'start and stop the other display timer.
	DisplayScoreQueue 0,0
End Sub

Sub MadTarget1Hit() 'target is hit
	If WhatMadTarget=1 Then Check1stMoveEek
	If WhatMadTarget=2 Then Check2ndMoveEek
	If WhatMadTarget=3 Then Check3rdMoveEek
	If WhatMadTarget=4 Then MadMiss
End Sub

Sub Check1stMoveEek()
	If MadTarget(1)=1 Then
		MadEek
	Else
		MadMiss
	End If
End Sub

Sub Check2ndMoveEek()
	If MadTarget(2)=1 Then
		MadEek
	Else
		MadMiss
	End If
End Sub

Sub Check3rdMoveEek()
	If MadTarget(3)=1 Then
		MadEek
	Else
		MadMiss
	End If
End Sub

Sub MadEek()
	PlaySound ""
	DisplayQueueScreen "      EEK       ","      EEK     ",8,8,0,500,True,""
	eekLight.State=LightStateOff
	HitBigChuck
End Sub

Sub MadTarget2Hit()
	If WhatMadTarget=1 Then Check1stMoveAck
	If WhatMadTarget=2 Then Check2ndMoveAck
	If WhatMadTarget=3 Then Check3rdMoveAck
	If WhatMadTarget=4 Then MadMiss
End Sub

Sub Check1stMoveAck()
	If MadTarget(1)=2 Then
		MadAck
	Else
		MadMiss
	End If
End Sub

Sub Check2ndMoveAck()
	If MadTarget(2)=2Then
		MadAck
	Else
		MadMiss
	End If
End Sub

Sub Check3rdMoveAck()
	If MadTarget(3)=2Then
		MadAck
	Else
		MadMiss
	End If
End Sub

Sub MadAck()
	PlaySound ""
	DisplayQueueScreen "      ACK       ","      ACK     ",8,8,0,500,True,""
	ackLight.State=LightStateOff
	HitBigChuck
End Sub

Sub MadTarget3Hit()
	If WhatMadTarget=1 Then Check1stMoveOok
	If WhatMadTarget=2 Then Check2ndMoveOok
	If WhatMadTarget=3 Then Check3rdMoveOok
	If WhatMadTarget=4 Then MadMiss
End Sub

Sub Check1stMoveOok()
	If MadTarget(1)=3 Then
		MadOok
	Else
		MadMiss
	End If
End Sub

Sub Check2ndMoveOok()
	If MadTarget(2)=3Then
		MadOok
	Else
		MadMiss
	End If
End Sub

Sub Check3rdMoveOok()
	If MadTarget(3)=3Then
		MadOok
	Else
		MadMiss
	End If
End Sub

Sub MadOok()
	PlaySound ""
	DisplayQueueScreen "      OOK       ","      OOK     ",8,8,0,500,True,""
	ookLight.State=LightStateOff
	HitBigChuck
End Sub

Sub MadMonkeyKicker()
	Select Case MadTarget(3)
		Case 1: EKick.Enabled=True: EekTrapper.IsDropped = 1  	' last target is Eek
		Case 2: AKick.Enabled=True: AckTrapper.IsDropped = 1 	' last target is Aak
		Case 3: OKick.Enabled=True: OokTrapper.IsDropped = 1 	' last target is Ook
	End Select
End Sub

Sub HitBigChuck()
	PlaySound "wallop"
	BigChuckTimer.Enabled=True
	LastMoveTimer.Enabled=False 'stop the show timers
	ShowLastMoveTimer.Enabled=False
	EekVel = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	LUpTimer.Enabled=True: MUpTimer.Enabled=True: RUpTimer.Enabled=True
	CorrectMadHits=CorrectMadHits+1
	If CorrectMadHits=3 Then
		CorrectMadSet=CorrectMadSet+1
		CorrectMadHits=0
		ModeLengthTimer.Enabled=False
	End If

	If CorrectMadHits=2 Then MadMonkeyKicker

	Select Case CorrectMadSet

	Case 3		 'last set complete
		DisplayQueueScreen "  EAAAARRRRRRR  ","  AAAAARRRRGHH  ",8,8,0,500,False,"chuck1"
		DisplayQueueScreen "     HURRY      "," SHOOT LE CHUCK ",3,3,0,1500,False,"init5"
		ModeTotal=ModeTotal+5000000
		If Play_Mode="Hard" Then ModeTotal=ModeTotal+5000000
		HurryUpWaitTimer.Enabled=True
		Chuck1Light.State=LightStateOn: Light24.State=LightStateBlinking
		MadAllowed=0

	Case 2		 '2nd set complete
		If CorrectMadHits=0 Then		' Display only when set complete
			ModeTotal=ModeTotal+5000000
			If MadCombatToDo > 2 Then
				DisplayQueueScreen "  AAAAARRRRRRR  ","  AAAAARRRRGH  ",8,8,0,500,False,"dargh"
				DisplayQueueScreen "    LE CHUCK    ","    DEFEATED   ",3,3,0,1500,False,""
				DisplayQueueScreen "      LAST      ","     ROUND     ",3,3,0,1500,False,"init5"
				MoreTime
				ResetMadStuff				' have to reset	all the variables
			Else
				DisplayQueueScreen "  EAAAARRRRRRR  ","  AAAAARRRRGHH  ",8,8,0,500,False,"chuck1"
				DisplayQueueScreen "     HURRY      "," SHOOT LE CHUCK ",3,3,0,1500,False,"init5"
				ModeTotalTimer.Enabled=false: HurryUpWaitTimer.Enabled=True
				Chuck1Light.State=LightStateOn: Light24.State=LightStateBlinking
				MadAllowed=0
			End If
		End If

	Case 1		 '1st set complete
		If CorrectMadHits=0 Then		' Display only when set complete
			DisplayQueueScreen "  AAAAARRRRRRR  ","  AAAAARRRRGH  ",8,8,0,500,False,"dargh"
			DisplayQueueScreen "    LE CHUCK    ","    DEFEATED   ",3,3,0,1500,False,""
			DisplayQueueScreen "      NEXT      ","     ROUND     ",3,3,0,1500,False,""
			ModeTotal=ModeTotal+5000000: MoreTime
			ResetMadStuff				' have to reset
		End If							' all the variables
	End Select
	DisplayScoreQueue 0,0
End Sub

Sub BigChuckTimer_Timer() 'pause to show mega lechuck hit
	BigChuckTimer.Enabled=False
End Sub

'************************* grave digger *****************************

Sub GraveDigger()
'shoot the combos in sequence to dig up the graves,
'if shot missed combos are reset.
'scores go up 2 million each shot
'sequence is:
'Scumm Loop - Banana Ramp - LeChuck - Captive Ball
	InsultOff
	CenterArrowsOff
	ScummKicker.TimerEnabled=True
	BallSave=1: ShootAgainLight.State=LightStateBlinking
	BallSaveTimer.Enabled=False		' turn off if allready active
	BallSaveTimer.Enabled=True
	Grave1Light.State=LightStateOn: Grave2Light.State=LightStateOn: Grave3Light.State=LightStateOn
	Status="GraveDigger"
	DisplayFlushQueue
	DisplayQueueScreen "     GRAVE      ","     DIGGER    ",6,7,0,1000,False,"mort1"
	DisplayQueueScreen "     GRAVE      ","     DIGGER    ",3,3,0,1000,False,""
	DisplayQueueScreen "     SHOOT      "," THE ENTRANCES ",0,0,0,1000,False,""
	DisplayQueueScreen "   TO DIG UP    ","   THE GRAVES  ",0,0,0,1000,True,""
	DisplayQueueScreen "                ","               ",2,1,0,1000,True,""
	GraveTarget=1: GraveTarget1=0: GraveTarget2=0: GraveTarget3=0
	InitGraveTimer.Enabled=True
End Sub

Sub InitGraveTimer_Timer()
	InitGraveTimer.Enabled=False
	NextGraveTarget
End Sub

Sub NextGraveTarget()
	ResetGraveTargets
	If GraveTarget=1 Then
		DisplayQueueScreen "      HIT       ","   SCUMMLOOP    ",3,3,0,1000,True,""
		Light43.State=LightStateOn
	End If
	If GraveTarget=2 Then
		DisplayQueueScreen "      HIT       ","  BANANA RAMP   ",3,3,0,1000,True,""
		Light83.State=LightStateOn
	End If
	If GraveTarget=3 Then
		DisplayQueueScreen "      HIT       ","    LECHUCK     ",3,3,0,1000,True,""
		Light24.State=LightStateOn
	End If
	If GraveTarget=4 Then
'		DisplayQueueScreen "      HIT       ","  MONKEY HEAD   ",3,3,0,1000,True,""
		DisplayQueueScreen "      HIT       ","  CAPTIVE BALL  ",3,3,0,1000,True,""
		Light82.State=LightStateOn
	End If
End Sub


Sub GotGraveTarget()
	DisplayFlushQueue
	If GraveTarget=1 Then
		DisplayQueueScreen "   SCUMMLOOP    ","      HIT       ",8,8,0,500,False,"hey"
		If GraveTarget1=0 Then
			DisplayQueueScreen "   " & GraveDiggerScores(1) & " GRAVES DUG ","                ",0,0,0,500,False,"CHAINS"
			DisplayQueueScreen "  GRAVE  AWARD  ","   " & GraveDiggerScores(1) & " MILLIONS   ",0,0,0,500,False,"CHAINS"
			GraveTarget1=1: MoreTime
			ModeTotal=ModeTotal+(GraveDiggerScores(1)*1000000)
		End If
	End If

	If GraveTarget=2 Then
		DisplayQueueScreen "  BANANA RAMP   ","      HIT       ",8,8,0,500,False,"hey"
		If GraveTarget2=0 Then
			DisplayQueueScreen "   " & GraveDiggerScores(2) & " GRAVES DUG ","                ",0,0,0,500,False,"CHAINS"
			DisplayQueueScreen "  GRAVE  AWARD  ","   " & GraveDiggerScores(2) & " MILLIONS   ",0,0,0,500,False,"CHAINS"
			GraveTarget2=1: MoreTime
			ModeTotal=ModeTotal+(GraveDiggerScores(2)*1000000)
		End If
	End If

	If GraveTarget=3 Then
		DisplayQueueScreen "    LECHUCK     ","      HIT       ",8,8,0,500,False,"hey"
		If GraveTarget3=0 Then
			DisplayQueueScreen "   " & GraveDiggerScores(3) & " GRAVES DUG ","                ",0,0,0,500,False,"CHAINS"
			DisplayQueueScreen "  GRAVE  AWARD  ","   " & GraveDiggerScores(3) & " MILLIONS   ",0,0,0,500,False,"CHAINS"
			GraveTarget3=1: MoreTime
			ModeTotal=ModeTotal+(GraveDiggerScores(3)*1000000)
		End If
	End If

	If GraveTarget=4 Then
		DisplayQueueScreen "  CAPTIVE BALL  ","      HIT      ",8,8,0,500,False,"hey"
		DisplayQueueScreen "   " & GraveDiggerScores(4) & " GRAVES DUG ","                ",0,0,0,500,False,"CHAINS"
		DisplayQueueScreen "  GRAVE  AWARD  ","   " & GraveDiggerScores(4) & " MILLIONS   ",0,0,0,500,False,"CHAINS"
		DisplayQueueScreen "     SHOOT      ","    LE CHUCK    ",3,3,0,500,False,"init5"
		ModeLengthTimer.Enabled=False 'stops main timer
		ModeTotal=ModeTotal+(GraveDiggerScores(4)*1000000)
		Light82.State=LightStateOff
		HurryUpWaitTimer.Enabled=True
	End If

	GraveTarget=GraveTarget+1 'shot number (so check 5 when hitting lechuck!!!)
	If GraveTarget < 5 Then NextGraveTarget
End Sub

Sub ResetGraveTargets()
	Light43.State=LightStateOff: Light24.State=LightStateOff
	Light82.State=LightStateOff: Light83.State=LightStateOff
End Sub

' ***************************** Treasure Hunt ************************************************
Sub TreasureHunt()
	LightArray(6, 1) = 1
	LightArray(25, 1) = 1
	SetPulse 6, 1
	SetPulse 25, 1
	Status="TreasureHunt"
	TH_ToDo=1
	DisplayFlushQueue()
	DisplayQueueScreen " TREASURE  HUNT "," TREASURE  HUNT ",2,1,0,1000,False,"goldhaha"
	DisplayQueueScreen " TREASURE  HUNT "," TREASURE  HUNT ",8,8,0,500,False,"coins"
	DisplayQueueScreen " SHOOT THE LIT  ","    ENTRANCES   ",0,0,0,1250,False,""
	DisplayQueueScreen "   TO GET THE   "," TREASURE ITEMS ",0,0,0,1250,False,""
	DisplayQueueScreen "  LEFT LOOP 2X  "," FOR THE SHOVEL ",3,3,0,1000,True,""
	CenterArrowsOff()
	LightArray(33, 1) = 2
	BallSave=1
	LightArray(22, 1) = 2
End Sub

Sub TreasureHunt_One()
	GrogKicker.TimerEnabled=True
	DisplayFlushQueue()
	DisplayQueueScreen "    YOU  GET    ","   THE SHOVEL   ",8,8,0,500,False,"efmi_fast3"
	DisplayQueueScreen " TREASURE BONUS ","   " & TreasureHuntScores(1) & " MILLIONS   ",0,3,0,1000,False,"coin"
	DisplayQueueScreen "    HIT  THE    ","   LIT  MONKEY  ",3,3,0,1500,False,""
	DisplayQueueScreen "   TO GET THE   ","  TREASURE MAP  ",3,3,0,1500,False,""
	DisplayQueueScreen "  EXTRA  TIME   ","    AWARDED     ",3,3,0,1000,True,"parrotcall"
	ModeTotal=ModeTotal+(TreasureHuntScores(1)*1000000)
	MoreTime()
	TH_ToDo=2
	MadTarget(3)=0
	MadTarget(1)=0
	TreasureHunt_MonkeyTimer.Enabled=True
End Sub

Sub TreasureHunt_MonkeyTimer_Timer()
	'EP- turn off Monkey Lights
	EekTrapper.IsDropped = 1:AckTrapper.IsDropped = 1:OokTrapper.IsDropped = 1
	MadTarget(1)=MadTarget(3)
	While MadTarget(3)=Madtarget(1): MadTarget(3)=round(rnd*2+1): Wend
	MadMonkeyKicker()
	If MadTarget(3)=1 Then
'		eekLight.State=LightStateBlinking
		PlaySound"Eek"
		EekVel = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
		LupTimer.Enabled=True
		PlaySound "monkeytargup"
	End if
	If MadTarget(3)=2 Then
'		ackLight.State=LightStateBlinking
		PlaySound"Ack"
		AckVel = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
		MupTimer.Enabled=True
	End If
	If MadTarget(3)=3 Then
'		ookLight.State=LightStateBlinking
		PlaySound"Oop"
		OokVel = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
		RupTimer.Enabled=True
	End If
End Sub

Sub TreasureHunt_Two()
	TreasureHunt_MonkeyTimer.Enabled=False
	'EP- Turn off Monkey Lights
	EekTrapper.IsDropped = 1:AckTrapper.IsDropped = 1:OokTrapper.IsDropped = 1
	DisplayFlushQueue()
	DisplayQueueScreen "  YOU FIND THE  ","  TREASURE MAP  ",8,8,0,500,False,"efmi_fast3"
	DisplayQueueScreen " TREASURE BONUS ","   " & TreasureHuntScores(2) & " MILLIONS   ",0,3,0,1000,False,"coin"
	DisplayQueueScreen "    HIT  THE    "," INSULT  LETTER ",3,3,0,1000,False,""
	DisplayQueueScreen "  TO FIND THE   ","     ISLAND     ",3,3,0,1000,False,""
	DisplayQueueScreen "  EXTRA  TIME   ","    AWARDED     ",3,3,0,1000,True,"parrotcall"
	ModeTotal=ModeTotal+(TreasureHuntScores(2)*1000000): MoreTime
	TH_ToDo=3: TH_Insult=0
	TreasureHunt_InsultTimer.Enabled=True
	CenterArrowsOff()
End Sub

Sub TreasureHunt_InsultTimer_Timer()
	InsultOff()
	x=TH_Insult
	While TH_Insult=x: TH_Insult=round(rnd*5+1): Wend
	Select Case TH_Insult
		Case 1: LastInsult="I": LightArray(56, 1) = 2
		Case 2: LastInsult="N": LightArray(57, 1) = 2
		Case 3: LastInsult="S": LightArray(58, 1) = 2
		Case 4: LastInsult="U": LightArray(59, 1) = 2
		Case 5: LastInsult="L": LightArray(60, 1) = 2
		Case 6: LastInsult="T": LightArray(61, 1) = 2
	End Select
End Sub

Sub TreasureHunt_Three
	TreasureHunt_InsultTimer.Enabled=False
	InitSMTimer()
	DisplayFlushQueue()
	DisplayQueueScreen "    YOU FIND    ","   THE ISLAND   ",8,8,0,1000,False,"efmi_fast3"
	DisplayQueueScreen " TREASURE BONUS ","   " & TreasureHuntScores(3) & " MILLIONS   ",0,3,0,1000,False,"coin"
	DisplayQueueScreen "   VISIT  THE   "," HOUSE OF MOJO  ",0,0,0,1500,False,""
	DisplayQueueScreen "    TO GRAB     ","  THE TREASURE  ",0,0,0,1500,False,""
	DisplayQueueScreen "  EXTRA  TIME   ","    AWARDED     ",3,3,0,1000,True,"parrotcall"
	ModeTotal=ModeTotal+(TreasureHuntScores(3)*1000000)
	TH_ToDo=4
	Kicker6.Enabled=True
	MoreTime()
	CenterArrowsOff()
	LightArray(46, 1) = 2
End Sub

Sub TreasureHunt_Four
	ModeLengthTimer.Enabled=False
	HurryUpWaitTimer.Interval=5000
	HurryUpWaitTimer.Enabled=True
	LightArray(50, 1) = 1
	LightArray(31, 1) = 2
	DisplayFlushQueue()
	DisplayQueueScreen " TREASURE FOUND "," TREASURE FOUND ",8,8,0,1000,False,"coins"
	DisplayQueueScreen " TREASURE BONUS ","   " & TreasureHuntScores(4) & " MILLIONS   ",0,3,0,1000,False,"coin"
	DisplayQueueScreen "     HURRY      "," SHOOT LE CHUCK ",3,3,0,1500,True,"init5"
	ModeTotal=ModeTotal+(TreasureHuntScores(4)*1000000)
	TH_ToDo=5
	Kicker6.TimerInterval=4500
	Kicker6.TimerEnabled=True
	CenterArrowsOff()
	LightArray(46, 1) = 0
End Sub

' ******************************* END OF MODES ***********************************************


Sub WaLeftSling_Slingshot()
	PlaySound "Bumper"
	If Status="GoEscape" Then
		If EscapeSwitch(1)=0 Then
			SwitchNumber=1
			SwitchOn()
			'EP- Blink Left Sling
		End If
	Else
		Playsound "doh"
		'EP- blink Left Sling
		AddScore(500)
		AddBonus(250)
	End If
End Sub

Sub WaRightSling_Slingshot()
	PlaySound "Bumper"
	If Status="GoEscape" Then
		If EscapeSwitch(2)=0 Then
			SwitchNumber=2
			SwitchOn
			'EP- Blink Right Sling
		End If
	Else
		Playsound "uuuh"
		'EP- Blink Right Sling
		AddScore(500)
		AddBonus(250)
	End If
End Sub

'***************************** voodoo magic *******************************************

Sub VooDooMagic()
'knock off 2 ball multi with captive item awards
'items:
'sea slug pate
'voodoo dolls
'essence of seagull
'monkey armpit juice
'cream of frog
	RovingMagnet.X = mAGNET2kicker.X
	RovingMagnet.Y = mAGNET2kicker.Y
	LightArray(8, 1) = 1
	SetPulse 8, 1
	DisplayFlushQueue
	DisplayQueueScreen "     VOODOO     ","      MAGIC     ",6,7,0,1000,False,"spooky"
	DisplayQueueScreen "     VOODOO     ","      MAGIC     ",8,8,0,1000,False,""
	DisplayQueueScreen "    HIT BALL    ","   TO RELEASE   ",0,0,0,500,False,""
	DisplayQueueScreen " CAPTIVE SHOTS  ","  AWARDS ITEMS  ",3,3,0,500,False,""
	ModeStartTimer.Enabled=True
	BallSave=1
	LightArray(22, 1) = 2
	LightArray(46, 1) = 2
	Status="VoodooMagic"
	VoodooItem=0
End Sub

Sub VooDooItemHit(WhoHit)
	Dim TempBall
	PlaySound "orbit"
	VooDooItem=VooDooItem+1
	DisplayFlushQueue()
	Select Case VooDooItem
	Case 1
		DisplayQueueScreen "  SEA SLUG PATE ","   2 MILLIONS   ",8,0,0,1000,True,"seaslugpate"
		ModeTotal=ModeTotal+2000000
	Case 2
		DisplayQueueScreen "  VOODOO DOLLS  ","   3 MILLIONS   ",8,0,0,1000,True,"dolls"
		ModeTotal=ModeTotal+3000000
	Case 3
		DisplayQueueScreen "  GULL ESSENCE  ","   4 MILLIONS   ",8,0,0,1000,True,"essenceofseagull"
		ModeTotal=ModeTotal+4000000
	Case 4
		DisplayQueueScreen "  MONKEY JUICE  ","   5 MILLIONS   ",8,0,0,1000,True,"monkeyarmpitjuice"
		ModeTotal=ModeTotal+5000000
	Case 5
		DisplayQueueScreen "  CREAM OF FROG ","   6 MILLIONS   ",8,0,0,1000,False,"creamoffrog"
		ModeTotal=ModeTotal+6000000
	Case 6	 'should not get here
		AddBonus(10000)
		VooDooItem=VooDooItem-1
	End Select
	If VoodooItem=VoodooItemToDo Then
		Kicker6.Enabled=False
		'Magnet2Kicker.Enabled = True
		VooDooProt.IsDropped = 0
		For Each X in Magic
			X.X = Kicker6.X
			X.Y = Kicker6.Y + 10
		Next
		PlaySound "ballinr"
'		BallsInPlay=BallsInPlay-1							'EP- Don't think this is needed until the VooDoo ball actually vanishes
		DisplayQueueScreen "     HURRY      "," SHOOT LE CHUCK ",3,3,0,1500,False,"init5"
		HurryUpWaitTimer.Enabled=True
		LightArray(49, 1) = 3
		MagnetsOffMain()
	End If
	If (VoodooItem=VoodooItemToDo - 1) Then Kicker6.Enabled=True
	DisplayScoreQueue 1,2
End Sub


Sub VoodooTarg_Hit()
	Select Case Status
	Case "Normal"
		NormalVoodoo()
	Case "VoodooMagic"
		VooDooItemHit(ActiveBall.ID)
	Case "GraveDigger"
		If GraveTarget=4 Then GotGraveTarget()
	Case "Multiball"
		DisplayFlushQueue
		DisplayQueueScreen " ADD TO JACKPOT ","     100K      ",0,0,0,500,True,"multihit1"
		PlaySound "clack"
		PlaySound "multihit1"
		Jackpot=Jackpot+100000
	Case "GoEscape"
		If EscapeSwitch(8)=0 Then
			LightArray(29, 1) = 2
			SwitchNumber=8
			SwitchOn()
		End If
	Case Else
		AddBonus(10000)
	End Select
	DisplayScoreQueue 0,0
End Sub

Sub ModeStartTimer_Timer() 'purely for voodoo mode
	ModeStartTimer.Enabled=False
	ActivateVooDooMagnet()
End Sub

Sub ActivateVooDooMagnet()
	PlaySound "ballinr"
	MagnetsOnMain()
	BallsinPlay=BallsinPlay+1
	LightArray(24, 1) = 3
End Sub

Sub MagnetsOnMain()		' NudgeOkay=0 because nudge liberate Voodoo Ball...
	Dim TmpBall
	LightArray(24, 1) = 3
	SetPulse 24, 1
	VooDooKeeper.IsDropped = 1
	MotorTimer.Enabled=True
	PlaySound "maghum",-1
'	mAGNET2kicker.kick 0, 0, 0
	Magnet2Kicker.enabled = False
	tmpBall = GetBalls
	For Each X in tmpBall
		If X.ID = 666 Then RovingMagnet.AddBall X
	Next
	RovingMagnet.MagnetOn = 1
End Sub

Sub MagnetsOffMain()
	LightArray(24, 1) = 0
	SetPulse 24, 0
	MagnetsOn = False
	MotorTimer.Enabled=False
	StopSound "maghum"
	RovingMagnet.MagnetOn = 0
	Gate4.Open = 0
	RovingMagnet.X = Magnet2Kicker.X
	RovingMagnet.Y = Magnet2Kicker.Y
End Sub

Dim VooDooSpeedX, VDC, voodooslope
voodooslope = ((Kicker9.Y - Magnet2kicker.Y)/(Kicker9.X - Magnet2Kicker.X))
VDC = (-((Kicker9.Y - Magnet2kicker.Y)/(Kicker9.X - Magnet2Kicker.X)) * Kicker9.X) + Kicker9.Y		'~12.5
VooDooSpeedX = 0.5
Sub MotorTimer_Timer()
	If RovingMagnet.X <= 150 Then Gate4.Open = 1
	RovingMagnet.X = RovingMagnet.X - VooDooSpeedX
	RovingMagnet.Y = (VooDooSlope * RovingMagnet.X) + VDC
'	For Each X in Magic
'		X.X = RovingMagnet.X
'		X.Y = RovingMagnet.Y
'	Next
	If RovingMagnet.Y < Kicker9.Y Then MagnetsOffMain
End Sub

Dim Voodoochecker
Sub Kicker9_Hit()
	Set voodoochecker = ActiveBall
	If Status = "VoodooMagic" Then GetRidOfVoodooBall()
End Sub

Sub GetRidOfVoodooBall()
	VooDooKeeper.IsDropped = 0
	If VooDooChecker.ID = 666 Then
		VooDooChecker.X = Magnet2Kicker.X
		VooDooChecker.Y = Magnet2Kicker.Y
		Set VooDooChecker = Nothing
	Else
		VooDooChecker.X = Drain.X
		VooDooChecker.Y = Drain.Y
	End If
	Kicker9.Kick 0, 0, 0
'	BallsinPlay=BallsinPlay-1					'EP- Taken care of in GoModeTotal
	MagnetsOffMain()
	Gate4.Open = 0
	BallSaveTimer.Enabled=False
	BallSave=0
	LightArray(22, 1) = 0
	DisplayFlushQueue
	DisplayQueueScreen "  VOODOO  BALL  ","    VANISHES    ",3,3,0,1000,False,""
	BallsInPlay = BallsInPlay - 1
	GoModeTotal()
	For Each X in Magic
		X.X = 500
		X.Y = -70
	Next
End Sub

Sub GoModeTotal()
	SetPulse 8, 0
	SetPulse 9, 0
	SetPulse 10, 0
	SetPulse 11, 0
	SetPulse 12, 0
	SetPulse 25, 0
	SetPulse 0, 0
	SetPulse 5, 0
	ModeLengthTimer.Enabled=False
	If ModeComplete=0 Then AllModesCompletedTimer.Interval=4000
	If ModesStarted=8 And BallsInPlay>0 Then AllModesCompletedTimer.Enabled=True
	BallSaveTimer.Enabled=False
	BallSave=0
	LightArray(22, 1) = 0
	If Status <> "Spitting" Then DisplayFlushQueue()
	If ModeComplete = 0 Then
		If HurryUp=1 Then
			HurryUpTimer.Enabled=False: HurryUpBonus=0
			DisplayQueueScreen "      ITEM      ","      LOST      ",3,3,0,500,False,""
			PlaySound "muhaha"
			HurryUp=0
			LightArray(49, 1) = 0
			LightArray(50, 1) = 0
			LightArray(31, 1) = 0
		End If
	End If
	Select Case Status
	Case "VoodooMagic" 		'voodoo
		If ModeComplete=0 Then DisplayQueueScreen "  VOODOO  BALL  ","    VANISHES    ",3,3,0,1000,False,""
		If ModeComplete=1 Then
			BallsInPlay = BallsInPlay - 1
			DisplayQueueScreen "  VOODOO  BALL  ","    VANISHES    ",3,3,0,1000,False,""
			If BallMover2.ID = 666 Then
				BallMover2.X = mAGNET2kicker.X
				BallMover2.Y = mAGNET2kicker.Y
			Else
				BallMover2.X = Drain.X
				BallMover2.Y = Drain.Y
			End If
		End If
		VooDooProt.IsDropped = 1
		For Each X in Magic
			X.X = 500
			X.Y = -70
		Next
		EndMusic()
		PlaySound "voodooend"
		Kicker6.Enabled=False
		LightArray(46, 1) = 0
	Case "Spitting" 		'spitting comp
		If ModeComplete=0 Then DisplayQueueScreen "  COMPETITION   ","    FINISHED    ",3,3,0,1000,False,"spit"
		If ModeComplete=1 Then DisplayQueueScreen "  COMPETITION   ","     WINNER     ",3,3,0,1000,False,"spit"
		WaSpit.IsDropped = 1
		GrogKicker.Enabled=False
		LightArray(41, 1) = 0
		GrogLight(-1)
		GiGrogTimer.Enabled = 0
		EndMusic()
		PlaySound "spitend"
	Case "HighDive" 		'high dive
		If ModeComplete=0 Then DisplayQueueScreen "   MARCO POLO   ","      WINS      ",3,3,0,1000,False,"marcowins"
		If ModeComplete=1 Then DisplayQueueScreen "    GUYBRUSH    ","      WINS      ",3,3,0,1000,False,"iwon"
		GrogKicker.Enabled=False
		LightArray(41, 1) = 0
		GrogLight(-1)
		GiGrogTimer.Enabled = 0
		EndMusic()
		PlaySound "drain1"
		PlaySound "diveend"
	Case "SwordMaster" 		'sword master
		If ModeComplete=0 Then DisplayQueueScreen "  SWORD MASTER  ","      WINS      ",3,3,0,1000,False,""
		If ModeComplete=1 Then DisplayQueueScreen "    GUYBRUSH    ","      WINS      ",3,3,0,1000,False,""	'"iwon"
		InsultOff()
		PlaySound "immildlyinsulted"
		PlaySound "swordend"
		EndMusic()
	Case "DaintyLady" 		'dainty lady
		If ModeComplete=0 Then DisplayQueueScreen "  NO AUTHORITY  ","     NO SHIP    ",3,3,0,1000,False,""
		If ModeComplete=1 Then DisplayQueueScreen "    CAPTAIN     ","   OF THE SEA   ",3,3,0,1000,False,"splash44"
		ResetShipTargets
		CenterArrowsFlash
		ShipMoveTimer.Enabled=False
		EndMusic()
		PlaySound "shipend2"
	Case "MonkeyCombat" 	'monkeycombat
		If ModeComplete=0 Then DisplayQueueScreen "  DEFEATED  BY  ","    LE CHUCK    ",3,3,0,1000,False,"muhaha"
		If ModeComplete=1 Then DisplayQueueScreen "    GUYBRUSH    ","      WINS      ",3,3,0,1000,False,"iwon"
		PlaySound "ultimateend"
		MadTargetTimer.Enabled=False
		NewMadTargetTimer.Enabled=False
		LastMoveTimer.Enabled=False
		ShowLastMoveTimer.Enabled=False
		AKick.Enabled=False
		EKick.Enabled=False
		OKick.Enabled=False
		BounceLTrig.Enabled=True
		BounceMTrig.Enabled=True
		BounceRTrig.Enabled=True
		ResetMonkeys
		EndMusic()
	Case "GraveDigger" 		'grave digger
		If ModeComplete=0 Then DisplayQueueScreen "  GRAVE DIGGER  ","      WINS      ",3,3,0,1000,False,"immildlyinsulted"
		If ModeComplete=1 Then DisplayQueueScreen "    GUYBRUSH    ","      WINS      ",3,3,0,1000,False,"iwon"
		PlaySound "swordend"
		ResetGraveTargets
		EndMusic()
	Case "TreasureHunt" 	'treasure hunt
		If ModeComplete=0 Then DisplayQueueScreen " LECHUCK STEALS ","  THE TREASURE  ",3,3,0,1000,False,"muhaha"
		If ModeComplete=1 Then DisplayQueueScreen "    TREASURE    ","    CHAMPION    ",3,3,0,1000,False,"coins"
		TreasureHunt_MonkeyTimer.Enabled=False
		PlaySound "Spitend"
		eekLight.State=LightStateOff
		ackLight.State=LightStateOff
		ookLight.State=LightStateOff
		EKick.Enabled=False
		AKick.Enabled=False
		OKick.Enabled=False
		BounceLTrig.Enabled=True
		BounceMTrig.Enabled=True
		BounceRTrig.Enabled=True
		Kicker6.Enabled=False
		VooDooMag.MagnetOn = 0
		MysteryLight.State=LightStateOff
		TreasureHunt_InsultTimer.Enabled=False
		InsultOff EndMusic()
	'esscape from MI dealt with in separate sub
	End Select
	Bananas=0
	Status="Normal"
	ModeComplete=0
	ActiveMonkeyBeat()
	TiScummKicker.Interval = 5000 	'reset the mode kicker back to normal
	MouthGuard.IsDropped=True 		'enable ball lock
	PlaySound "solon"
	CenterArrowsFlash()
	LightArray(49, 1) = 0
	LightArray(50, 1) = 0
	LightArray(51, 1) = 0
	If VoodooCount=3 Then 			' restore Mystery
		Kicker6.Enabled=True
		LightArray(46, 1) = 0
	End If
	If GrogHits > 2 Then 			'turn the grog machine back on if ready
		GiGrogTimer.Enabled = 1
		GrogKicker.Enabled=True
		LightArray(43, 1) = 2
		LightArray(44, 1) = 2
		LightArray(45, 1) = 2
		If Status = "Normal" Then GrogKicker.Enabled = 1
	End If
	If GrogHits = 2 Then
		LightArray(43, 1) = 1
		LightArray(44, 1) = 1
	End If
	If GrogHits = 1 Then LightArray(43, 1) = 1
	If Tilt=1 Then ModeTotal=0 		'TILT!
	DisplayQueueScreen "   MODE TOTAL   ","   "&FormatScore(ModeTotal)&"   ",0,0,0,1000,True,""
	AddScore(ModeTotal)
	ModeTotalTimer.Enabled=True
	DisplayScoreQueue 1,2
End Sub

Sub ModeTotalTimer_Timer() 'gives a little delay so mode total is shown before drain routine
	ModeTotalTimer.Enabled=False
	ModeTotal=0
	BallStop=0
	If Song <> "EFMI_MonkeyTheme.mp3" Then
		If BallsinPlay>0 Then Song="EFMI_MonkeyTheme.mp3": PlayMusic Song
	End If
	If BallsinPlay<1 Then FinishDrain()
End Sub

' ************************************* TRIGGERS ******************************************

Sub LeftLoopTrig_Hit():  LoopEntrance="Left":  StopSound "stoneloop": End Sub
Sub RightLoopTrig_Hit(): LoopEntrance="Right": StopSound "stoneloop": End Sub

Sub Trigger18_Hit() 'upper left
	If LoopEntrance="Left" Then
		StopSound "stoneloop"
		PlaySound "stoneloop",-1
	Else
		StopSound "stoneloop"
		PlaySound "stoneout"
		Select Case Status
			Case "Normal"
				If ComboMult=1 Then
					AddMulti()
				Else
					ComboMult=1
					ComboMultiTimer.Enabled=True
				End If
			End Select
	End If
End Sub

Sub Trigger6_Hit()
	If Status="Normal" Then
		DisplayQueueScreen "   SHOOT  THE   ","   LEFT  LOOP   ",0,0,0,1000,False,""
		DisplayQueueScreen "    TO  LITE    ","  GROG MACHINE  ",0,0,0,1000,False,""
		DisplayQueueScreen "  BARREL VALUE  "," = "&FormatScore(BarrelScore)&"  ",3,3,0,1500,True,""
		DisplayScoreQueue 1,2
	End If
End Sub

Sub Trigger14_Hit() 'outside loop - middle trig
	'PlaySound "stoneloop"
	Select Case Status
	Case "Normal"
		If LoopEntrance="Left" Then
			If ComboActive=1 Then
				AddCombo
			Else
				ComboActive=1
				CenterArrowsOn()
				ComboTimer.Enabled=True
				AddScore(3750): AddBonus(5000)
			End If
		CheckGrog()
		AddScore(1750)
		End If
		If LoopEntrance="Right" Then
			DisplayFlushQueue()
			DisplayQueueScreen "   RIGHT LOOP   ","                ",1,2,0,1200,True,""
			DisplayScoreQueue 1,2
			LightArray(48, 1) = 2
			If ComboActive=1 Then
				AddCombo()
			Else
				ComboActive=1
				CenterArrowsOn()
				ComboTimer.Enabled=True
				AddScore(2000): AddBonus(5000)
			End If
		AddScore(1750)
		End If
	Case "DaintyLady"
		If LoopEntrance="Left" And ShipTarget=1 Then GotShipTarget()
		If LoopEntrance="Right" And ShipTarget=5 Then GotShipTarget()
	Case "GoEscape"
		Select Case LoopEntrance
			Case "Left"
				If EscapeSwitch(14)=0 Then
					LightArray(33, 1) = 1
					SwitchNumber=14
					SwitchOn()
				End If
			Case "Right"
				If EscapeSwitch(15)=0 Then
					LightArray(41, 1) = 1
					SwitchNumber=15
					SwitchOn()
				End If
		End Select
	Case "TreasureHunt"
		If TH_ToDo=1 Then
			If LoopEntrance="Left" Then
				If ComboActive=1 Then
					GrogKicker.Enabled=True
				Else
					ComboActive=1 'no combos during modes
					DisplayFlushQueue()
					DisplayQueueScreen "      ONCE      ","      MORE      ",3,3,0,1500,True,""
					LightArray(33, 1) = 1
					ComboTimer.Enabled=True
				End If
			End If
		End If
	End Select
End Sub

Sub Trigger13_Hit() 'banana loop
	If Status <> "MadMonkey" Then
		If Status <> "GoEscape" Then
			GoBananas()
		End If
	Else
		If BananaPicker=1 Then
			Bananas=Bananas+1
		End If
	End If
	If Status = "Normal" Then
		ComboActive=1
		CenterArrowsOn()
		ComboTimer.Enabled=True
		DisplayScoreQueue 1,2
	End If
End Sub


Sub Trigger19_Hit() 'upper right
	If LoopEntrance="Right" Then
		StopSound "stoneloop"
		PlaySound "stoneloop",-1
	Else
		StopSound "stoneloop"
		PlaySound "stoneout"
	End If
End Sub

Sub Trigger16_Hit() 'barrel out
	PlaySound "ballroll2"
	Select Case Status
	Case "Normal"
		If ComboActive=1 Then
			AddCombo()
		End If
	Case "DaintyLady"
		IF ShipTarget=4 Then
			GotShipTarget()
		End If
	Case "GraveDigger"
		If GraveTarget=2 Then
			GotGraveTarget()
		End If
	Case "GoEscape"
		If EscapeSwitch(9)=0 Then
			LightArray(28, 1) = 1
			SwitchNumber=9
			SwitchOn()
		End If
	End Select
	DisplayScoreQueue 0,0
End Sub

Sub Spinner1_Spin()
	PlaySound "tin can"
	AddScore(500)
	BarrelScore=BarrelScore+10
	AddBonus(50)
End Sub

Sub Trigger1_Hit()
	PlaySound "wallop"
	AddScore(1500)
	If LightArray(1, 1) = 1 OR LightArray(1, 1) = 2 OR LightArray(1, 1) = 3 Then
		LightArray(1, 1) = 0
		LightArray(3, 1) = 0
		Credits=Credits+1
		DisplayQueueScreen "    SPECIAL     ","   EXTRA BALL   ",8,8,0,1000,False,"knocker"
	End If
End Sub

Sub Trigger2_Hit()
	PlaySound "wallop"
	AddScore(1500)
	If LightArray(3, 1) = 1 OR LightArray(3, 1) = 2 OR LightArray(3, 1) = 3 Then
		LightArray(1, 1) = 0
		LightArray(3, 1) = 0
		DisplayQueueScreen "    SPECIAL     ","   EXTRA BALL   ",8,8,0,1000,False,"knocker"
		Credits=Credits+1
	End If
End Sub



Sub Trigger7_Hit():  DiverterR.IsDropped=False: StopSound "stoneloop": LoopEntrance="": End Sub

Sub Trigger12_Hit() 'scumm loop
	'EP- Some light FX in the middle
	Select Case Status
	Case "Normal"
		AddBonus(5000)
		If ComboActive=1 Then
			AddCombo()
		Else
			ComboActive=1
			CenterArrowsOn()
			ComboTimer.Enabled=True
		End If
		GoScummLoop()
	Case "Multiball"
		If LightArray(39, 1) = 2 Then
			If Super=0 Then
				If SpinningDirection=0 Then 'stops incorrect displays/music if head is moving
					MBLit()
				End If
			Else
				GoSuperInfo()
			End If
		Else
			DisplayScoreQueue 0,0
		End If
	Case "DaintyLady"
		IF ShipTarget=3 Then
			GotShipTarget()
		End If
	Case "GraveDigger"
		IF GraveTarget=1 Then
			GotGraveTarget()
		End If
	Case "GoEscape"
		If EscapeSwitch(13)=0 Then
			LightArray(42, 1) = 2
			SwitchNumber=13
			SwitchOn()
		End If
	End Select
End Sub

Sub GoScummLoop()
	DisplayFlushQueue
	ScummLoops=ScummLoops+1
	AddScore(3500)
	Select Case ScummLoops
	Case 1	 'increase bonus
		AddBonus(500000)
		DisplayQueueScreen "     BONUS      ","   INCREASES    ",3,3,0,1000,False,"HEY"
		DisplayQueueScreen " BONUS + 500000 ","   "&FormatScore(Bonus),0,8,0,1000,False,"efmi_fast3"
		NextScummAward=3
		ScummTarget.IsDropped=False
		ScummTarget.TimerEnabled=True
	Case 3	 '5 bananas
		If Bananas < 5 Then Bananas=Bananas+1
		DisplayQueueScreen "  + 1 BANANAS   ","                ",5,4,0,500,False,"haveabanana"
		DisplayQueueScreen "  BANANAS = "&Bananas,"                ",0,0,0,1000,False,""
		NextScummAward=6
		ScummTarget.IsDropped=False
		ScummTarget.TimerEnabled=True
	Case 6	 'extra ball lit
		DisplayQueueScreen "   EXTRA BALL   ","     IS LIT     ",8,8,0,1500,False,"knocking"
		NextScummAward=10
		LightArray(40, 1) = 2
		EB=1
		ScummKicker.Enabled=True
		ScummTarget.IsDropped=False
		ScummTarget.TimerEnabled=True
	Case 10	'bonus multiplier
		AddMulti()
		NextScummAward=15
		ScummTarget.IsDropped=False
		ScummTarget.TimerEnabled=True
	Case 15 'Monkey Head
		DisplayQueueScreen "  AWARD MONKEY  ","    HEAD HIT    ",3,3,0,1500,False,""
		NextScummAward=21
		HeadRoutine()
		ScummTarget.IsDropped=False
		ScummTarget.TimerEnabled=True
	Case 21 'Big points
		DisplayQueueScreen "   BIG POINTS   ","   5 MILLIONS   ",3,3,0,1000,False,"coin"
		AddScore(5000000)
		NextScummAward=28
		ScummTarget.IsDropped=False
		ScummTarget.TimerEnabled=True
	Case 28 '+25 barrels
		DisplayQueueScreen "      + 25      ","    BARRELS    ",3,3,0,1500,False,"burp2"
		NextScummAward=36
		Barrels=Barrels+25
		ScummTarget.IsDropped=False
		ScummTarget.TimerEnabled=True
	Case 36 'Playfield x3
		DisplayQueueScreen "   PLAYFIELD    ","      X 3      ",8,8,0,1500,False,"3headedmonkey"
		NextScummAward=45
		PFMultiplier=3
		PFMultiplierLight.TimerEnabled=True
		LightArray(21, 1) = 1
		ScummTarget.IsDropped=False
		ScummTarget.TimerEnabled=True
	Case 45 'Bigger points
		DisplayQueueScreen " BIGGER POINTS  ","   15 MILLIONS  ",3,3,0,1000,False,"coins"
		AddScore(15000000)
		NextScummAward=55
		ScummTarget.IsDropped=False
		ScummTarget.TimerEnabled=True
	Case 55 '+1 bananas
		If Bananas < 5 Then Bananas=Bananas+1
		DisplayQueueScreen "  + 1 BANANAS   ","                ",5,4,0,500,False,"haveabanana"
		DisplayQueueScreen "  BANANAS = "&Bananas,"                ",0,0,0,1000,False,""
		NextScummAward=75
		ScummTarget.IsDropped=False
		ScummTarget.TimerEnabled=True
	Case 75 'ExtraBall
		DisplayQueueScreen "   EXTRA BALL   ","   EXTRA BALL   ",8,8,0,1500,False,"explode1"
		ExtraBall()
		NextScummAward=100
		ScummTarget.IsDropped=False
		ScummTarget.TimerEnabled=True
	Case 100 'Light Next Mode to do I think
		DisplayQueueScreen "   LIGHT NEXT   ","      MODE      ",3,3,0,1500,False,"init5"
		ModesLit()
		NextScummAward=1000
		ScummTarget.IsDropped=False: ScummTarget.TimerEnabled=True
	End Select
	DisplayQueueScreen "      "&ScummLoops,"  SCUMM  LOOPS  ",0,0,0,1000,False,""
	DisplayQueueScreen "   NEXT AWARD   ","      AT "&NextScummAward,0,0,0,1000,True,""
	DisplayScoreQueue 2,1
	BarrelScore=BarrelScore+1500
End Sub

Sub ScummTarget_Hit(): PlaySound "shakegrog": End Sub

Sub ScummTarget_Timer()
	ScummTarget.TimerEnabled=False
	ScummTarget.IsDropped=True
	PlaySound "diverter"
End Sub

Sub GoBananas() ' ::)) sic!
	AddBonus(5000)
	DisplayFlushQueue()
	PlaySound "ballroll"
	If BananaPicker=0 Then	'haven't got a picker yet
		DisplayQueueScreen "    GET THE    "," BANANA PICKER  ",3,3,0,1000,True,""
		AddScore(25000)
		AddBonus(2500)
		Exit Sub
	End If
	Bananas=Bananas+1		' we have banana picker...
	Select Case Bananas
	Case 1
		DisplayQueueScreen "   "&Bananas&" BANANAS "," (              ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," )              ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," (              ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," )              ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," (              ",0,0,0,125,False,"haveabanana"
		DisplayQueueScreen "   "&Bananas&" BANANAS "," )              ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," (              ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," )              ",0,0,0,125,False,""

	Case 2
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( (            ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) )            ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( (            ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) )            ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( (            ",0,0,0,125,False,"haveabanana"
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) )            ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( (            ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) )            ",0,0,0,125,False,""

	Case 3
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( (          ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) )          ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( (          ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) )          ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( (          ",0,0,0,125,False,"haveabanana"
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) )          ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( (          ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) )          ",0,0,0,125,False,""

	Case 4
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) ) )        ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( ( (        ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) ) )        ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( ( (        ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) ) )        ",0,0,0,125,False,"haveabanana"
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( ( (        ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) ) )        ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( ( (        ",0,0,0,125,False,""

	Case 5
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( ( ( (      ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) ) ) )      ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( ( ( (      ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) ) ) )      ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( ( ( (      ",0,0,0,125,False,"haveabanana"
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) ) ) )      ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( ( ( (      ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) ) ) )      ",0,0,0,125,False,""

	Case 6
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( ( ( ( (    ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) ) ) ) )    ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( ( ( ( (    ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) ) ) ) )    ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( ( ( ( (    ",0,0,0,125,False,"haveabanana"
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) ) ) ) )    ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( ( ( ( (    ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) ) ) ) )    ",0,0,0,125,False,""

	Case 7
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( ( ( ( ( (  ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) ) ) ) ) )  ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( ( ( ( ( (  ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) ) ) ) ) )  ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( ( ( ( ( (  ",0,0,0,125,False,"haveabanana"
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) ) ) ) ) )  ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ( ( ( ( ( ( (  ",0,0,0,125,False,""
		DisplayQueueScreen "   "&Bananas&" BANANAS "," ) ) ) ) ) ) )  ",0,0,0,125,False,""

	Case 8 'error
	End Select
	DisplayQueueScreen "  GRAB BANANAS "," FOR EXTRA TIME ",3,3,0,1000,True,""
	AddScore(Bananas*100000)
	AddBonus(Bananas*10000)
End Sub

Sub Trigger3_Hit()
	PlaySound "doorclose"
	AddBonus(1000)
	If Status="GoEscape" Then
		If EscapeSwitch(20)=0 Then
			SwitchNumber=20
			SwitchOn
		End If
	End If
End Sub

Sub Trigger4_Hit()
	PlaySound "doorclose"
	AddBonus(1000)
	If Status="GoEscape" Then
		If EscapeSwitch(19)=0 Then
			SwitchNumber=19
			SwitchOn
		End If
	End If
End Sub

Sub HurryUpTimer_Timer()
	HurryUpTimer.Enabled=False
	HurryUp=1
	DisplayFlushQueue
	DisplayHurryUp()
	PlaySound "meterplink"
	HurryUpBonus=HurryUpBonus-1
	If HurryUpBonus=-1 Then
		HurryUp=0
		LightArray(50, 1) = 0
		LightArray(31, 1) = 0
		DisplayFlushQueue
		DisplayQueueScreen "      ITEM      ","      LOST      ",1,2,0,1500,False,"muhaha"
		DisplayQueueScreen "      ITEM      ","      LOST      ",3,3,0,1500,False,""
		HurryUpEndTimer.Enabled=True
	Else
		HurryUpTimer.Enabled=True
	End If
End Sub

Sub HurryUpWaitTimer_Timer()
	HurryUpWaitTimer.Enabled=False
	HurryUpWaitTimer.Interval=3000
	EndMusic()
	HurryUpBonus=Round(300*CoeffTimer)+HurryUpBonus
	If HurryUpBonus > MaxHurryUp Then HurryUpBonus=MaxHurryUp
	HurryUpTimer.Enabled=True
End Sub

Sub HurryUpEndTimer_Timer()
	HurryUpEndTimer.Enabled=False
	DisplayFlushQueue
	GoModeTotal
End Sub

Sub MagnetLight2_Timer()
	MagnetLight2.TimerEnabled=False
	MagnetsOffMain
	Wall72.IsDropped=False 'close the voodoo trap door
End Sub

' ******************************* BOUNCING MONKEY TARGETS ****************************************

' LEFT

Sub EEKTarg_Hit()
	Select Case Status
	Case "GoEscape"
		If EscapeSwitch(5)=0 Then
			'EP- Some lighting for the Left Monkey target
			SwitchNumber=5
			SwitchOn()
		End If
	Case "MonkeyCombat"
		LBounce=0
		EekVel = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
		LUpTimer.Enabled=True
		PlaySound "monkeytargup"
		If MadAllowed=1 Then
			WhatMadTarget=WhatMadTarget+1
			PlaySound "bigeek"
			MadTargetNumber=1
			MadTarget1Hit()
		End If
	Case Else
		LBounce=0
		EekVel = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
		LUpTimer.Enabled=True
		PlaySound "monkeytargup"
		DisplayFlushQueue()
		If Combat1Count=10 Then
			DisplayQueueScreen "  COMBAT BONUS  ","   1  MILLION   ",3,8,0,1000,True,"GUYEEK"
			Addscore(10000000): AddBonus(3500)
			PlaySound "bigstep"
		Else
			DisplayQueueScreen "      EEK       ","      EEK       ",8,8,0,1000,True,"Eek"
			Combat1Count=Combat1Count+1
			CheckCombat()
		End If
	End Select
End Sub

Dim Dir1, chdir1, updown1, slowmo
slowmo = 1'.98												'Make this number lower for slow-mo Monkeys
Dir1 = 1
updown1 = 1
ChDir1 = 0
Dim EekVel:EekVel = 0
Dim EekDir:EekDir = 1
Sub LUpTimer_Timer()
	dim rotdir
	'If it has come down and has stopped moving then let it start moving.  This prevents double hits as the monkey shouldn't jump again while it's still in the air
	If updown1 = -1 AND ChDir1 = 0 Then ChDir1 = 1
	'If it's on it's way up, then...
	If ChDir1 = 1 Then
		'If it "hit the glass", play a sound and send it back down
		If PrEekMonkey.Z >= 200 Then
			PlaySound "knocker", 0, LVL(0.1), -1, 0
			ChDir1 = 2
		End If
	End If
	'Move the monkey according to Maths
	PrEekMonkey.Z = dSin(dir1) * EekVel * 10 + 120
	'If the monkey has rotated left, then start it rotating right, else start it rotating left
	If PrEekMonkey.Rotz > 20 then
		EekDir = -1
	ElseIf PrEekMonkey.rotz < -55 Then
		EekDir = 1
	End If
	'simple rotation
	PrEekMonkey.RotZ = PrEekMonkey.RotZ + (EekVel * 0.05 * EekDir)

	If dir1 >= 80 Then updown1 = -1
	dir1 = dir1 + dCos(dir1) * updown1 * slowmo
	'If the monkey landed, make sure it's at it's resting spot and turn off the animatino
	If PrEekMonkey.Z <= 119 Then
		PrEekMonkey.Z = 120
		Me.Enabled = 0
		Dir1 = 1
		ChDir1 = 0
		updown1 = 1
	End If
End Sub

' MID
Sub AckTarg_Hit()
	If Status="GoEscape" Then
		If EscapeSwitch(6)=0 Then
			'EP- Some lighting for the Center Monkey target
			SwitchNumber=6
			SwitchOn()
		End If
	Else
		MBounce=0
		AckVel = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
		MUpTimer.Enabled=True
		If Status="MonkeyCombat" Then
			If MadAllowed=1 Then
				WhatMadTarget=WhatMadTarget+1
				PlaySound "bigack"
				MadTargetNumber=2
				MadTarget2Hit()
			End If
		Else 'mode=other (normal)
			DisplayFlushQueue()
			If Combat2Count=10 Then
				DisplayQueueScreen "  COMBAT BONUS  ","   1  MILLION   ",3,8,0,1000,True,"GUYACK"
				Addscore(10000000): AddBonus(3500)
				PlaySound "bigstep"
			Else
				DisplayQueueScreen "      ACK       ","      ACK       ",8,8,0,1000,True,"Ack"
				Combat2Count=Combat2Count+1
				CheckCombat()
			End If
		End If
	End If
End Sub

Dim Dir2, chdir2, updown2
Dir2 = 1
updown2 = 1
ChDir2 = 0
Dim AckVel:AckVel = 0
Dim AckDir:AckDir = 1
Sub MUpTimer_Timer()
	dim rotdir
	'If it has come down and has stopped moving then let it start moving.  This prevents double hits as the monkey shouldn't jump again while it's still in the air
	If updown2 = -1 AND ChDir2 = 0 Then ChDir2 = 1
	'If it's on it's way up, then...
	If ChDir2 = 1 Then
		'If it "hit the glass", play a sound and send it back down
		If PrAckMonkey.Z >= 200 Then
			PlaySound "knocker", 0, LVL(0.1), 0, 0
			ChDir2 = 2
		End If
	End If
	'Move the monkey according to Maths
	PrAckMonkey.Z = dSin(dir2) * AckVel * 10 + 120
	'If the monkey has rotated left, then start it rotating right, else start it rotating left
	If PrAckMonkey.Rotz > 20 then
		AckDir = -1
	ElseIf PrAckMonkey.rotz < -55 Then
		AckDir = 1
	End If
	'simple rotation
	PrAckMonkey.RotZ = PrAckMonkey.RotZ + (AckVel * 0.05 * AckDir)
	If dir2 >= 80 Then updown2 = -1
	dir2 = dir2 + dCos(dir2) * updown2 * slowmo
	'If the monkey landed, make sure it's at it's resting spot and turn off the animatino
	If PrAckMonkey.Z <= 119 Then
		PrAckMonkey.Z = 120
		Me.Enabled = 0
		Dir2 = 1
		ChDir2 = 0
		updown2 = 1
	End If
End Sub

' RIGHT
Sub OOKTarg_Hit()
	If Status="GoEscape" Then
		If EscapeSwitch(7)=0 Then
			'EP- Some lighting for the Right Monkey target
			SwitchNumber=7
			SwitchOn()
		End If
	Else
		RBounce=0
		OokVel = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
		RUpTimer.Enabled=True
		If Status="MonkeyCombat" Then
			If MadAllowed=1 Then
				WhatMadTarget=WhatMadTarget+1
				PlaySound "bigoop"
				MadTargetNumber=3
				MadTarget3Hit()
			End If
		Else 'mode=other (normal)
			DisplayFlushQueue()
			If Combat3Count=10 Then
				DisplayQueueScreen "  COMBAT BONUS  ","   1  MILLION   ",3,8,0,1000,True,"GUYOOP"
				Addscore(10000000)
				AddBonus(3500)
				PlaySound "bigstep"
			Else
				DisplayQueueScreen "      OOK       ","      OOK       ",8,8,0,1000,True,"Oop"
				Combat3Count=Combat3Count+1
				CheckCombat()
			End If
		End If
	End If
End Sub

Dim Dir3, chdir3, updown3
Dir3 = 1
updown3 = 1
ChDir3 = 0
Dim OokVel:OokVel = 0
Dim OokDir:OokDir = 1
Sub RUpTimer_Timer()
	dim rotdir
	'If it has come down and has stopped moving then let it start moving.  This prevents double hits as the monkey shouldn't jump again while it's still in the air
	If updown3 = -1 AND ChDir3 = 0 Then ChDir3 = 1
	'If it's on it's way up, then...
	If chdir3 = 1 Then
		'If it "hit the glass", play a sound and send it back down
		If PrOokMonkey.Z >= 200 Then
			PlaySound "knocker", 0, LVL(0.1), 1, 0
			chdir3 = 2
		End If
	End If
	'Move the monkey according to Maths
	PrOokMonkey.Z = dSin(Dir3) * OokVel * 10 + 120
	'If the monkey has rotated left, then start it rotating right, else start it rotating left
	If PrOokMonkey.Rotz > 20 then
		OkkDir = -1
	ElseIf PrOokMonkey.rotz < -55 Then
		OokDir = 1
	End If
	'simple rotation
	PrOokMonkey.RotZ = PrOoMonkey.RotZ + (OokVel * 0.05 * OokDir)
	If dir3 >= 80 Then updown3 = -1
	Dir3 = dir3 + dCos(dir3) * updown3 * slowmo
	'If the monkey landed, make sure it's at it's resting spot and turn off the animatino
	If PrOokMonkey.Z <= 119 Then
		PrOokMonkey.Z = 120
		Me.Enabled = 0
		Dir3 = 1
		chdir3 = 0
		updown3 = 1
	End If
End Sub

Sub CheckCombat()
	AddScore(8000): AddBonus(1000)
	x=Combat1Count*Combat2Count*Combat3Count
	If Status = "Normal" Then
		If x>0 Then
			AKick.Enabled=True: EKick.Enabled=True: OKick.Enabled=True
			CombatTimer.Enabled=True
			CombatLevel=CombatLevel+1
			DefeatDisplay()
		End If
	DisplayScoreQueue 0,0
	Else
		If x>0 Then
			DisplayFlushQueue()
			DisplayQueueScreen "     BONUS +    ","      25000     ",0,0,0,500,True,""
			AddBonus(25000)
			CombatTimer.Enabled=True
			Combat1Count=0
			Combat2Count=0
			Combat3Count=0
		End If
	End If
End Sub

Sub DefeatDisplay()
	EEKTrapper.IsDropped = 0
	OOKTrapper.IsDropped = 0
	ACKTrapper.IsDropped = 0
	Select Case CombatLevel
		Case 1: CombatAward="500000"
		Case 2: CombatAward="750000"
		Case 3: CombatAward="1000000"
		Case 4: CombatAward="2000000"
		Case 5: CombatAward="4000000"
		Case 6: CombatAward="6000000"
		Case 7: CombatAward="8000000"
		Case 8: CombatAward="10000000"
		Case 9: CombatAward="12500000"
	End Select
	'EP- blink all 3 monkey lights
	'EP- Animate all three Monkeys
	DisplayFlushQueue
	DisplayQueueScreen "O              O","                ",0,0,0,20,False,"bigstep"
	DisplayQueueScreen " O            O ","-",0,8,0,20,False,""
	DisplayQueueScreen "  O          O  ","-",0,8,0,20,False,""
	DisplayQueueScreen "   O        O   ","-",0,8,0,20,False,""
	DisplayQueueScreen "    O      O    ","-",0,8,0,20,False,""
	DisplayQueueScreen "     O    O     ","-",0,8,0,20,False,""
	DisplayQueueScreen "      O  O      ","-",0,8,0,20,False,""
	DisplayQueueScreen "       OO       ","-",0,8,0,20,False,""
	DisplayQueueScreen "      O  O      ","-",0,8,0,20,False,""
	DisplayQueueScreen "     O    O     ","-",0,8,0,20,False,""
	DisplayQueueScreen "    O      O    ","-",0,8,0,20,False,""
	DisplayQueueScreen "   O        O   ","-",0,8,0,20,False,""
	DisplayQueueScreen "  O          O  ","-",0,8,0,20,False,""
	DisplayQueueScreen " O            O ","-",0,8,0,20,False,""
	DisplayQueueScreen "O              O","-",0,8,0,20,False,""
	If CombatLevel=10 Then
		DisplayQueueScreen " COMBAT   LEVEL ","       10       ",8,8,0,500,False,""
		DisplayQueueScreen " BEAT MODE LIT  ","  SHOOT LECHUCK ",3,3,0,1500,False,"init5"
		MonkeyBeat=1
		ActiveMonkeyBeat()
		CombatLevel=0
	Else
		DisplayQueueScreen "     MONKEY     ","    DEFEATED    ",8,8,0,500,False,""
		DisplayQueueScreen " COMBAT LEVEL "&CombatLevel&" ","   "&FormatScore(CombatAward)&"    ",0,0,0,1500,True,"bigstep"
	End If
	AddScore(CombatAward)
	Combat1Count=0
	Combat2Count=0
	Combat3Count=0
	DisplayScoreQueue 0,0
End Sub

Sub CombatTimer_Timer()
	CombatTimer.Enabled=False
	'EP- Turn off the Monkey Lights
	EEKTrapper.IsDropped = 1
	OOKTrapper.IsDropped = 1
	ACKTrapper.IsDropped = 1
	PlaySound "diverter"
	AKick.Enabled=False
	EKick.Enabled=False
	OKick.Enabled=False
	'EP- Turn Off monkey animations
End Sub

Sub ActiveMonkeyBeat()
	If Status="Normal" And MonkeyBeat=1 Then
		LightArray(51, 1) = 1
	Else
	 	LightArray(51, 1) = 0
	End If
End Sub

' ************************************ KICKERS

Sub EKick_Hit()
	'EP- Bounce Monkey
	EekTrapper.IsDropped = 0
	If Status="MonkeyCombat" Then
		If MadAllowed=1 Then
			WhatMadTarget=WhatMadTarget+1
			PlaySound "bigeek"
			MadTargetNumber=1
			If CorrectMadSet=(MadCombatToDo-1) And CorrectMadHits = 2 Then
				EKick.TimerInterval=1500	' last set, kicker must be faster
			Else
				EKick.TimerInterval=8500	' firsts sets have to see displays
			End If
			EKick.TimerEnabled = True
			MadTarget1Hit()
		End If
	End If
	If Status="TreasureHunt" And TH_ToDo=2 Then
		If MadTarget(3)=1 Then
			Set BallSM = ActiveBall
			EKick.TimerInterval=6000
			EKick.TimerEnabled = True
			ModeLengthTimer.Enabled=False
			TreasureHunt_Two()
		End If
	End If
End Sub

Sub AKick_Hit()
	AckTrapper.IsDropped = 0
	If Status="MonkeyCombat" Then
		If MadAllowed=1 Then
			WhatMadTarget=WhatMadTarget+1
			PlaySound "bigaak"
			MadTargetNumber=2
			If CorrectMadSet=(MadCombatToDo-1) And CorrectMadHits = 2 Then
				AKick.TimerInterval=1500	' last set, kicker must be faster
			Else
				AKick.TimerInterval=8500	' firsts sets have to see displays
			End If
			AKick.TimerEnabled = True
			MadTarget2Hit()
		End If
	End If
	If Status="TreasureHunt" And TH_ToDo=2 Then
		If MadTarget(3)=2 Then
			Set BallSM = ActiveBall
			AKick.TimerInterval=6000
			AKick.TimerEnabled = True
			ModeLengthTimer.Enabled=False
			TreasureHunt_Two()
		End If
	End If
End Sub

Sub OKick_Hit()
	OokTrapper.IsDropped = 0
	If Status="MonkeyCombat" Then
		If MadAllowed=1 Then
			WhatMadTarget=WhatMadTarget+1
			PlaySound "bigook"
			MadTargetNumber=3
			If CorrectMadSet=(MadCombatToDo-1) And CorrectMadHits = 2 Then
				OKick.TimerInterval=1500	' last set, kicker must be faster
			Else
				OKick.TimerInterval=8500	' firsts sets have to see displays
			End If
			OKick.TimerEnabled = True
			MadTarget3Hit()
		End If
	End If
	If Status="TreasureHunt" And TH_ToDo=2 Then
		If MadTarget(3)=3 Then
			Set BallSM = ActiveBall
			OKick.TimerInterval=6000
			OKick.TimerEnabled = True
			ModeLengthTimer.Enabled=False
			TreasureHunt_Two()
		End If
	End If
End Sub

Sub EKick_Timer()
	EKick.TimerEnabled = False
	EKick.TimerInterval=5000
	EekTrapper.IsDropped = 1
	PlaySound "ballinr"
	EKick.Enabled=False
	If Status="TreasureHunt" Then ModeLengthTimer.Enabled=True
End Sub

Sub AKick_Timer()
	AKick.TimerEnabled = False
	AKick.TimerInterval=5000
	AckTrapper.IsDropped = 1
	PlaySound "ballinr"
	AKick.Enabled=False
	If Status="TreasureHunt" Then ModeLengthTimer.Enabled=True
End Sub

Sub OKick_Timer()
	OKick.TimerEnabled = False
	OKick.TimerInterval=5000
	OokTrapper.IsDropped = 1
	PlaySound "ballinr"
	OKick.Enabled=False
	If Status="TreasureHunt" Then ModeLengthTimer.Enabled=True
End Sub


'******************************* COMBOS ********************************

Sub ComboTimer_Timer()
	ComboTimer.Enabled=False
	CenterArrowsFlash()
	LightArray(48, 1) = 0
	ComboActive=0
	If Status="TreasureHunt" Then
		If TH_ToDo=1 Then
			DisplayFlushQueue
			DisplayQueueScreen "  COMBO MISSED  ","   TRY  AGAIN   ",3,3,0,1000,True,"doh"
			CenterArrowsOff()
			LightArray(33, 1) = 2
		End If
	End If
End Sub

Sub AddCombo()
	Combos=Combos+1
	ComboValue=Combos*500000
	DisplayFlushQueue
	DisplayQueueScreen "  " &FormatScore1K(Combos,"","") & " COMBOS    ","  " &FormatScore(ComboValue) &"     ",0,0,0,500,False,"hey"
	DisplayQueueScreen "  " &FormatScore1K(Combos,"","") & " COMBOS    ","  " &FormatScore(ComboValue) &"     ",0,8,0,500,True,""
	AddScore(ComboValue)
	ComboTimer.Enabled=False
	ComboActive=0
	CenterArrowsFlash()
	DisplayScoreQueue 2,1
End Sub

Sub ComboMultiTimer_Timer()
	ComboMultiTimer.Enabled=False
	ComboMult=0
	LightArray(48, 1) = 0
End Sub

Sub AddMulti()
	If BonusMultiplier = 1 Then
		BonusMultiplier = 2
	Else
		BonusMultiplier = BonusMultiplier + 2
	End If
	Select Case BonusMultiplier
		Case "2": 	LightArray(14, 1) = 1
		Case "4": 	LightArray(15, 1) = 1: LightArray(14, 1) = 0
		Case "6": 	LightArray(16, 1) = 1: LightArray(15, 1) = 0
		Case "8": 	LightArray(17, 1) = 1: LightArray(16, 1) = 0
		Case "10": 	LightArray(18, 1) = 1: LightArray(17, 1) = 0
		Case "12": 	LightArray(19, 1) = 1: LightArray(18, 1) = 0
		Case "14":
		If EBScoredThisBall=0 Then
			DisplayQueueScreen "   EXTRA BALL   ","     IS LIT     ",8,8,0,1500,False,"knocking"
			LightArray(20, 1) = 2
			EB=1
		End If
	End Select
	If BonusMultiplier = 14 Then BonusMultiplier = 12
	DisplayFlushQueue
	DisplayQueueScreen "     BONUS      ","   MULTIPLER    ",0,0,0,500,False,"explode1"
	DisplayQueueScreen "   * BONUS *    ","      X "&BonusMultiplier,0,8,0,1000,True,""
	ComboMultiTimer.Enabled=False
	ComboMult=0
	LightArray(48, 1) = 1
	DisplayScoreQueue 2,1
End Sub


'**************************************************
'
'   Generic Williams Display Driver Functions
'                Version 2.0
'
'     Written By Chris Leathley (ala Black)
'
'  Can be freely used in other tables providing
'              credit is given
'
'This script can be modified by the user but
'any modified scripts are to remain with the
'owner and NOT reposted as enhancements to the
'the display driver
'
'Requires the following elements on the table
'--------------------------------------------
'
'Timers called 'DisplayTimer'
'          and 'DisplayEffectTimer'
'Variables named 'BallsPerGame' which contains
'                the number of balls per game
'          and 'BallsRemaining' which contains
'              the number of balls remaining
'              this game
'
'revision history
'----------------
'
'v2.0  Added effect enums
'      Added in Scroll Out and Scroll in Effects
'      Added Fast Blink Effect
'      Added in Trail In Multiplier Effect
'      Added in Blink Mask effects
'      Limititations of effects combinations removed
'      Added length checking to DisplayQueueScreen()
'      Fixed a few progmatic errors
'      DisplayInit() API changed
'
'v1.1  Added Format Score functions for use with
'      'WilliamsPinball.ttf' font
'      Added sound functionality to displays
'
'v1.0  First Released with Black Knight 2000
'
'**************************************************
'**************************************************

'Option Explicit                   ' Force explicit variable declaration.
' user defined constants
Const dcCHARSPERLINE    = 16      ' Number of characters per text line (Williams/Bally default is 16)
' define effect constants (enum's)
Const eNone             = 0       ' Instantly displayed
Const eScrollLeft       = 1       ' scroll on from the right
Const eScrollRight      = 2       ' scroll on from the left
Const eBlink            = 3       ' Blink (blinks for 'TimeOn') at user specified intervals (slow speed)
							  ' blink speed (either line) in milliseconds (default 100ms) must be multiple of dcEFFECTSPEED
							  ' it takes 2*n to do a full cycle (n on and n off). DisplayQueueScreen TimeOn should be multiple
							  ' of 2*n to ensure a smooth effect transition
Const eScrollOut        = 4       ' scroll out from middle to the edges
Const eScrollIn         = 5       ' scroll in the edges to the middle
Const eScrollLeftOver   = 6       ' scroll on from the right (over current text)
Const eScrollRightOver  = 7       ' scroll on from the left (over current text)
Const eBlinkFast        = 8       ' Blink (blinks for 'TimeOn') at user specified intervals (fast speed)
Const eTrailIn          = 9       ' Trail in towards the middle (usefull for only 2 characters in the middle)
Const eBlinkMask        = 10      ' Blinks only non space text in text over current text line (slow speed)
Const eBlinkMaskFast    = 11      ' Blinks only non space text in text over current text line (slow speed)

Dim DispQueueSize                 ' size of display queue (64)
Dim DispQueueHead                 ' head of queue (current display)
Dim DispQueueTail                 ' tail of queue (where to put new ones)
Dim DisplayBlankLine
Dim DispCurrentTopLine
Dim DispCurrentBottomLine
Dim DispEffectCountT1
Dim DispEffectCountT1End
Dim DispEffectBlinkCycleT1
Dim DispEffectCountT2
Dim DispEffectCountT2End
Dim DispEffectCountT2Dly
Dim DispEffectSpeed
Dim DispEffectBlinkSlowRate
Dim DispEffectBlinkFastRate
Dim DispEffectBlinkCycleT2
Dim DispQueueText1(64)            ' storage elements of the queue (would much prefer a structure)
Dim DispQueueText2(64)
Dim DispQueueEffectOnT1(64)
Dim DispQueueEffectOnT2(64)
Dim DispQueueEffectOnT2Dly(64)
Dim DispQueueTimeOn(64)
Dim DispQueuebFlush(64)
Dim DispQueueSound(64)

'initialise the display driver
Sub DisplayInit(EffectSpeed, SlowBlinkRate, QuickBlinkRate)
	Dim i
	' define the queue size and flush it
	DispQueueSize = 64
	DisplayFlushQueue()
	' set the specified parameters
	DispEffectSpeed         = EffectSpeed
	DispEffectBlinkSlowRate = SlowBlinkRate
	DispEffectBlinkFastRate = QuickBlinkRate
	' set up the black line semi constant
	DisplayBlankLine = ""
	For i = 1 To dcCHARSPERLINE
	DisplayBlankLine = DisplayBlankLine & " "
	Next
	' blank out the 2 lines on the display
	DispCurrentTopLine    = DisplayBlankLine
	DispCurrentBottomLine = DisplayBlankLine
	TextBoxTop.Text    = DispCurrentTopLine
	TextBoxBottom.Text = DispCurrentBottomLine
End Sub

'flush the display queue, dosn't change the display in anyway
Sub DisplayFlushQueue()
	' stop the display timers
	DisplayTimer.Enabled = False
	DisplayEffectTimer.Enabled = False
	' no displays in the queue
	DispQueueHead = 0
	DispQueueTail = 0
	' and certainly no effects
	DispEffectCountT1 = 0
	DispEffectCountT1End = 0
	DispEffectBlinkCycleT1 = 0
	DispEffectCountT2 = 0
	DispEffectCountT2End = 0
	DispEffectCountT2Dly = 0
	DispEffectBlinkCycleT2 = 0
End Sub

'displays the score screen immediatly
Sub DisplayScoreNow()
	' flush any other display's queued up and call the display score function
	DisplayFlushQueue(): DisplayScore()
End Sub

'display the current score and ball number (writes directly to the screen)
Sub DisplayScore()
	Dim Balls
	Dim TempTopStr
	Dim TempBottomStr
	If Tilt=1 Then Exit Sub
	' the queue must be empty for the score screen to be displayed
	If (DispQueueHead = DispQueueTail) And (HurryUpBonus = 0) Then
		TempTopStr = FormatScore(Score)&"      "
		DispCurrentTopLine = TempTopStr
		TextBoxTop.Text    = TempTopStr
		Balls = BallsPerGame + 1: Balls = Balls - BallsRemaining
		Select Case Status
		Case "Normal"
			If BallsRemaining<>0 Then
				TempBottomStr = "         BALL "&Balls&" "
			Else
				TempTopStr =	"      GAME      "
				TempBottomStr =	"      OVER      "
			End If
		Case "VoodooMagic"
			TempTopStr = " *VOODOO MAGIC* ": TempBottomStr = "    "&FormatScore(Score)&"    "
		Case "Spitting"
			TempTopStr = "  * SPITTING *  ": TempBottomStr = "   TIME = "&ModeTimeLeft&"  "
		Case "HighDive"
			TempTopStr = " * HIGH DIVE *  ": TempBottomStr ="   TIME = "&ModeTimeLeft&"  "
		Case "DaintyLady"
			TempTopStr = "  *DAINTY LADY* ": TempBottomStr ="   TIME = "&ModeTimeLeft&"  "
		Case "SwordMaster"
			TempTopStr = " *SWORD MASTER* ": TempBottomStr ="   TIME = "&ModeTimeLeft&"  "
		Case "MonkeyCombat"
			TempTopStr = "*MONKEY COMBAT* ": TempBottomStr ="   TIME = "&ModeTimeLeft&"  "
		Case "GraveDigger"
			TempTopStr = " *GRAVE DIGGER* ": TempBottomStr ="   TIME = "&ModeTimeLeft&"  "
		Case "Multiball"
			TempTopStr = "   *MULTIBALL*  ": TempBottomStr ="    "&FormatScore(Score)&"    "
		Case "GoEscape"
			TempTopStr = " *ESCAPE MODE*  "
			TempBottomStr =" "&EscapeNb&" HITS TO DO"
		Case "MonkeyBeat"
			TempTopStr = "     MONKEY     ": TempBottomStr ="      BEAT      "
		End Select
		DispCurrentTopLine 		= TempTopStr
		DispCurrentBottomLine 	= TempBottomStr
		TextBoxTop.Text    		= TempTopStr
		TextBoxBottom.Text    	= TempBottomStr
	End If
End Sub

'this function queues up the score display with the specified effect(s)..
'this is so a scroll can scroll back onto to default score screen
Sub DisplayScoreQueue(EffectOnT1, EffectOnT2)
	Dim Balls
	Dim TempTopStr
	Dim TempBottomStr
	TempTopStr = FormatScore(Score)&"       "
	Balls = BallsPerGame + 1
	Balls = Balls - BallsRemaining
	' "          BALL #"
	TempBottomStr = "         BALL "&Balls&" "
	If Balls > 0 Then DisplayQueueScreen TempTopStr, TempBottomStr, EffectOnT1, EffectOnT2, 0, 25, True, ""
End Sub

'this function returns the number of outstanding displays in the queue
Function DisplayGetQueueSize()
	DisplayGetQueueSize = DispQueueTail - DispQueueHead
End Function

'This function will add in a display screen into the queue and fire it off if it is
'the first display in the queue
Sub DisplayQueueScreen(Text1, Text2, EffectOnT1, EffectOnT2, DelayT2, TimeOn, bFlush, Sound)
	' must be room in the queue
	If (DispQueueTail < DispQueueSize) Then
		If (Text1 = "-") Then EffectOnT1 = eNone
		If (Text2 = "-") Then EffectOnT2 = eNone
		'save the details in the queue
		DispQueueText1(DispQueueTail)         = Text1
		DispQueueText2(DispQueueTail)         = Text2
		DispQueueEffectOnT1(DispQueueTail)    = EffectOnT1
		DispQueueEffectOnT2(DispQueueTail)    = EffectOnT2
		DispQueueEffectOnT2Dly(DispQueueTail) = DelayT2
		DispQueueTimeOn(DispQueueTail)        = TimeOn
		DispQueuebFlush(DispQueueTail)        = bFlush
		DispQueueSound(DispQueueTail)         = Sound
		'move the to next queue slot
		DispQueueTail = DispQueueTail + 1
		'if this is the first thing is the queue, Then start the ball rolling (so to speak ;-)
		If (DispQueueTail = 1) Then DisplayHead()
	End If
End Sub

'Display the screen at the head of the queue
Sub DisplayHead()
	' calculate the length of the effect required for the screen
	' set the start values for the effects
	DispEffectCountT1 = 0: DispEffectCountT2 = 0
	' set timer interval
	DisplayEffectTimer.Interval = DispEffectSpeed
	' set delay between top line effect and bottom line effect (delay is n * effect time interval)
	DispEffectCountT2Dly = DispQueueEffectOnT2Dly(DispQueueHead)
	Select Case (DispQueueEffectOnT1(DispQueueHead))
Case eNone:             DispEffectCountT1End = 1                                  ' Instantly Display (no effect)
Case eScrollLeft:       DispEffectCountT1End = Len(DispQueueText1(DispQueueHead)) ' effect loops
Case eScrollRight:      DispEffectCountT1End = Len(DispQueueText1(DispQueueHead))
Case eBlink:            DispEffectCountT1End = int(DispQueueTimeOn(DispQueueHead) / DispEffectSpeed)
						DispEffectBlinkCycleT1 = 0
Case eScrollOut:        DispEffectCountT1End = Len(DispQueueText1(DispQueueHead))/2
Case eScrollIn:         DispEffectCountT1End = Len(DispQueueText1(DispQueueHead))/2
Case eScrollLeftOver:   DispEffectCountT1End = Len(DispQueueText1(DispQueueHead))
Case eScrollRightOver:  DispEffectCountT1End = Len(DispQueueText1(DispQueueHead))
Case eBlinkFast:        DispEffectCountT1End = int(DispQueueTimeOn(DispQueueHead) / DispEffectSpeed)
						DispEffectBlinkCycleT1 = 0
Case eTrailIn:          DispEffectCountT1End = Len(DispQueueText1(DispQueueHead))/2
						DispCurrentTopLine = DisplayBlankLine
Case eBlinkMask:        DispEffectCountT1End = int(DispQueueTimeOn(DispQueueHead) / DispEffectSpeed)
						DispEffectBlinkCycleT1 = 0
Case eBlinkMaskFast:    DispEffectCountT1End = int(DispQueueTimeOn(DispQueueHead) / DispEffectSpeed)
						DispEffectBlinkCycleT1 = 0
	End Select
	Select Case (DispQueueEffectOnT2(DispQueueHead))
Case eNone:             DispEffectCountT2End = 1
Case eScrollLeft:       DispEffectCountT2End = Len(DispQueueText2(DispQueueHead))
Case eScrollRight:      DispEffectCountT2End = Len(DispQueueText2(DispQueueHead))
Case eBlink:            DispEffectCountT2End = int(DispQueueTimeOn(DispQueueHead) / DispEffectSpeed)
						DispEffectBlinkCycleT2 = 0
Case eScrollOut:        DispEffectCountT2End = Len(DispQueueText2(DispQueueHead))/2
Case eScrollIn:         DispEffectCountT2End = Len(DispQueueText2(DispQueueHead))/2
Case eScrollLeftOver:   DispEffectCountT2End = Len(DispQueueText2(DispQueueHead))
Case eScrollRightOver:  DispEffectCountT2End = Len(DispQueueText2(DispQueueHead))
Case eBlinkFast:        DispEffectCountT2End = int(DispQueueTimeOn(DispQueueHead) / DispEffectSpeed)
						DispEffectBlinkCycleT2 = 0
Case eTrailIn:          DispEffectCountT2End = Len(DispQueueText2(DispQueueHead))/2
						DispCurrentBottomLine = DisplayBlankLine
Case eBlinkMask:        DispEffectCountT2End = int(DispQueueTimeOn(DispQueueHead) / DispEffectSpeed)
						DispEffectBlinkCycleT2 = 0
Case eBlinkMaskFast:    DispEffectCountT2End = int(DispQueueTimeOn(DispQueueHead) / DispEffectSpeed)
						DispEffectBlinkCycleT2 = 0
	End Select
	' if there is a sound for this screen Then play it
	If (DispQueueSound(DispQueueHead) <> "") Then PlaySound(DispQueueSound(DispQueueHead))
	' start the effect timer
	DisplayEffectTimer.Enabled = True
End Sub

'The Effect Timer Has Expired, process the effect routine
Sub DisplayEffectTimer_Timer()
	' stop the timer
	DisplayEffectTimer.Enabled = False
	' process the effect
	DisplayProcessEffectOn()
End Sub

'the 'TimeOn' time has expired for the current (head) display
'process the OFF effect
Sub DisplayTimer_Timer()
	Dim Head
	' stop the timer
	DisplayTimer.Enabled = False
	' save the current head pointer
	Head = DispQueueHead
	' move to the next display in the queue (if there is one)
	DispQueueHead = DispQueueHead + 1
	' if head equal tail than at the end of queue Then
	' restart the from the begining of the queue
	If (DispQueueHead = DispQueueTail) Then
		' does this display want to flush the queue?
		If (DispQueuebFlush(Head) = True) Then
			' yep
			DisplayFlushQueue()
			' return to the default screen which is the score display
			DisplayScore()
			' leave the timer off
		Else
			' start again from the begining
			DispQueueHead = 0
			' and display it
			DisplayHead()
		End If
	Else
		' display the next screen
		DisplayHead()
	End If
End Sub

'This Function actually draws the text to the display using the effect spefified
'
'effects: eNone             0= Instantly displayed
'          eScrollLeft       1= scroll on from the left
'          eScrollRight      2= scroll on from the right
'          eBlink            3= Blink (blinks for 'TimeOn') at slow speed
'          eScrollOut        4= scroll out from middle to edges
'          eScrollIn         5= scroll in from the edges to the middle
'          eScrollLeftOver   6= scroll over from the left
'          eScrollRightOver  7= scroll over from the right
'          eBlinkFast        8= Blink (blinks for 'TimeOn') at fast speed
'          eTrailIn          9= Trails in middle 2 characters
'          eBlinkMask        10= Blink only active text in line (over current line)
'          eBlinkMaskFast    11= Blink only active text in line (over current line)
'
Sub DisplayProcessEffectOn()
	Dim i
	Dim BlinkEffect
	Dim TempTopStr
	Dim TempBottomStr
	Dim TempLeftStr
	Dim TempRightStr
	Dim MaskCharacter
	BlinkEffect = False
	' process the first line (T1)
	TempLeftStr = "":TempRightStr = ""
	' notch one up on the effect cycle count (Text Line 1) providing we havn't already finished
	If (DispEffectCountT1 <> DispEffectCountT1End) Then
		DispEffectCountT1 = DispEffectCountT1 + 1
	' manipulate the line according to the effect
	select case (DispQueueEffectOnT1(DispQueueHead))
	case eNone:
		TempTopStr = DispQueueText1(DispQueueHead)
  case eScrollLeft:
	TempTopStr = Right(DispCurrentTopLine, dcCHARSPERLINE-1)
	TempTopStr = TempTopStr & Mid(DispQueueText1(DispQueueHead), DispEffectCountT1 ,1)

  case eScrollRight:
	TempTopStr = Mid(DispQueueText1(DispQueueHead), (dcCHARSPERLINE+1)-DispEffectCountT1 ,1)
	TempTopStr = TempTopStr & Left(DispCurrentTopLine, dcCHARSPERLINE-1)

  case eBlink:
	BlinkEffect = True
	If ((DispEffectCountT1 MOD DispEffectBlinkSlowRate) = 0) Then
	  DispEffectBlinkCycleT1 = DispEffectBlinkCycleT1 xor 1
	End If
	If (DispEffectBlinkCycleT1 = 0) Then
	  TempTopStr = DispQueueText1(DispQueueHead)
	Else
	  TempTopStr = DisplayBlankLine
	End If

  case eScrollOut:
	TempLeftStr  = Mid(DispCurrentTopLine, 2, (dcCHARSPERLINE/2)-1)
	TempLeftStr  = TempLeftStr & Mid(DispQueueText1(DispQueueHead), DispEffectCountT1, 1)
	TempRightStr = Mid(DispQueueText1(DispQueueHead), (dcCHARSPERLINE+1)-DispEffectCountT1, 1)
	TempRightStr = TempRightStr & Mid(DispCurrentTopLine, (dcCHARSPERLINE/2)+1, (dcCHARSPERLINE/2)-1)
	TempTopStr   = TempLeftStr & TempRightStr

  case eScrollIn:
	TempLeftStr  = Mid(DispQueueText1(DispQueueHead), ((dcCHARSPERLINE/2)+1)-DispEffectCountT1, 1)
	TempLeftStr  = TempLeftStr & Left(DispCurrentTopLine, (dcCHARSPERLINE/2)-1)
	TempRightStr = Right(DispCurrentTopLine, (dcCHARSPERLINE/2)-1)
	TempRightStr = TempRightStr & Mid(DispQueueText1(DispQueueHead), (dcCHARSPERLINE/2)+DispEffectCountT1, 1)
	TempTopStr   = TempLeftStr & TempRightStr

  case eScrollLeftOver:
	TempTopStr = Left(DispCurrentTopLine, dcCHARSPERLINE-DispEffectCountT1)
	TempTopStr = TempTopStr & Left(DispQueueText1(DispQueueHead), DispEffectCountT1)

  case eScrollRightOver:
	TempTopStr = Right(DispQueueText1(DispQueueHead), DispEffectCountT1)
	TempTopStr = TempTopStr & Right(DispCurrentTopLine, dcCHARSPERLINE-DispEffectCountT1)

  case eBlinkFast:
	BlinkEffect = True
	If ((DispEffectCountT1 MOD DispEffectBlinkFastRate) = 0) Then
	  DispEffectBlinkCycleT1 = DispEffectBlinkCycleT1 xor 1
	End If
	If (DispEffectBlinkCycleT1 = 0) Then
	  TempTopStr = DispQueueText1(DispQueueHead)
	Else
	  TempTopStr = DisplayBlankLine
	End If

  case eTrailIn:
	TempLeftStr = left(DispCurrentTopLine, DispEffectCountT1-1)
	TempLeftStr = TempLeftStr & Mid(DispQueueText1(DispQueueHead), (dcCHARSPERLINE/2), 1)
	TempLeftStr = TempLeftStr & Mid(DispCurrentTopLine, DispEffectCountT1+1, (dcCHARSPERLINE/2)-DispEffectCountT1)
	TempRightStr = Mid(DispCurrentTopLine, (dcCHARSPERLINE/2)+1, (dcCHARSPERLINE/2)-DispEffectCountT1)
	TempRightStr = TempRightStr & Mid(DispQueueText1(DispQueueHead), (dcCHARSPERLINE/2)+1, 1)
	TempRightStr = TempRightStr & right(DispCurrentTopLine, DispEffectCountT1-1)
	TempTopStr = TempLeftStr & TempRightStr

  case eBlinkMask
	TempTopStr = ""
	BlinkEffect = True
	If ((DispEffectCountT1 MOD DispEffectBlinkSlowRate) = 0) Then
	  DispEffectBlinkCycleT1 = DispEffectBlinkCycleT1 xor 1
	End If
	For i = 1 To dcCHARSPERLINE
	  MaskCharacter = Mid(DispQueueText1(DispQueueHead), i, 1)
	  If (MaskCharacter <> " ") Then
		If (DispEffectBlinkCycleT1 = 0) Then
		  TempTopStr = TempTopStr & MaskCharacter
		Else
		  TempTopStr = TempTopStr & " "
		End If
	  Else
		TempTopStr = TempTopStr & Mid(DispCurrentTopLine, i, 1)
	  End If
	next

  case eBlinkMaskFast
	TempTopStr = ""
	BlinkEffect = True
	If ((DispEffectCountT1 MOD DispEffectBlinkFastRate) = 0) Then
	  DispEffectBlinkCycleT1 = DispEffectBlinkCycleT1 xor 1
	End If
	For i = 1 To dcCHARSPERLINE
	  MaskCharacter = Mid(DispQueueText1(DispQueueHead), i, 1)
	  If (MaskCharacter <> " ") Then
		If (DispEffectBlinkCycleT1 = 0) Then
		  TempTopStr = TempTopStr & MaskCharacter
		Else
		  TempTopStr = TempTopStr & " "
		End If
	  Else
		TempTopStr = TempTopStr & Mid(DispCurrentTopLine, i, 1)
	  End If
	Next

End Select

' if the text is "-" Then leave this line alone
If (DispQueueText1(DispQueueHead) <> "-") Then
  If (Len(TempTopStr) <> dcCHARSPERLINE) and (Len(TempTopStr) <> 0) Then
'MsgBox "DISPLAY PROBLEM" & vbNewLine & vbNewLine & "INTERNAL ERROR: STRING TOO LONG" & vbNewLine & "TempTopStr '" & TempTopStr & "' Len=" & Len(TempTopStr), 0, "Black Display Driver Error"
  End If
  DispCurrentTopLine = TempTopStr
  TextBoxTop.Text = TempTopStr
End If
	End If

' process the second line (T2)
TempLeftStr = "":TempRightStr = ""

	If (DispEffectCountT2 <> DispEffectCountT2End) Then
' notch one up on the effect cycle count (Text Line 1)
		If (DispEffectCountT2Dly = 0) Then
			DispEffectCountT2 = DispEffectCountT2 + 1

' manipulate the line according to the effect
  select case (DispQueueEffectOnT2(DispQueueHead))
	case eNone:
	  TempBottomStr = DispQueueText2(DispQueueHead)

	case eScrollLeft:
	  TempBottomStr = Right(DispCurrentBottomLine, dcCHARSPERLINE-1)
	  TempBottomStr = TempBottomStr & Mid(DispQueueText2(DispQueueHead), DispEffectCountT2 ,1)

	case eScrollRight:
	  TempBottomStr = Mid(DispQueueText2(DispQueueHead), (dcCHARSPERLINE+1)-DispEffectCountT2 ,1)
	  TempBottomStr = TempBottomStr & Left(DispCurrentBottomLine, dcCHARSPERLINE-1)

	case eBlink:
	  BlinkEffect = True
	  If ((DispEffectCountT2 MOD DispEffectBlinkSlowRate) = 0) Then
		DispEffectBlinkCycleT2 = DispEffectBlinkCycleT2 xor 1
	  End If
	  If (DispEffectBlinkCycleT2 = 0) Then
		TempBottomStr = DispQueueText2(DispQueueHead)
	  Else
		TempBottomStr = DisplayBlankLine
	  End If

	case eScrollOut:
	  TempLeftStr  = Mid(DispCurrentBottomLine, 2, (dcCHARSPERLINE/2)-1)
	  TempLeftStr  = TempLeftStr & Mid(DispQueueText2(DispQueueHead), DispEffectCountT2, 1)
	  TempRightStr = Mid(DispQueueText2(DispQueueHead), (dcCHARSPERLINE+1)-DispEffectCountT2, 1)
	  TempRightStr = TempRightStr & Mid(DispCurrentBottomLine, (dcCHARSPERLINE/2)+1, (dcCHARSPERLINE/2)-1)
	  TempBottomStr = TempLeftStr & TempRightStr

	case eScrollIn:
	  TempLeftStr  = Mid(DispQueueText2(DispQueueHead), ((dcCHARSPERLINE/2)+1)-DispEffectCountT2, 1)
	  TempLeftStr  = TempLeftStr & Left(DispCurrentBottomLine, (dcCHARSPERLINE/2)-1)
	  TempRightStr = Right(DispCurrentBottomLine, (dcCHARSPERLINE/2)-1)
	  TempRightStr = TempRightStr & Mid(DispQueueText2(DispQueueHead), (dcCHARSPERLINE/2)+DispEffectCountT2, 1)
	  TempBottomStr = TempLeftStr & TempRightStr

	case eScrollLeftOver:
	  TempBottomStr = Left(DispCurrentBottomLine, dcCHARSPERLINE-DispEffectCountT2)
	  TempBottomStr = TempBottomStr & Left(DispQueueText2(DispQueueHead), DispEffectCountT2)

	case eScrollRightOver:
	  TempBottomStr = Right(DispQueueText2(DispQueueHead), DispEffectCountT2)
	  TempBottomStr = TempBottomStr & Right(DispCurrentBottomLine, dcCHARSPERLINE-DispEffectCountT2)

	case eBlinkFast:
	  BlinkEffect = True
	  If ((DispEffectCountT2 MOD DispEffectBlinkFastRate) = 0) Then
		DispEffectBlinkCycleT2 = DispEffectBlinkCycleT2 xor 1
	  End If
	  If (DispEffectBlinkCycleT2 = 0) Then
		TempBottomStr = DispQueueText2(DispQueueHead)
	  Else
		TempBottomStr = DisplayBlankLine
	  End If

	case eTrailIn:
	  TempLeftStr = left(DispCurrentBottomLine, DispEffectCountT2-1)
	  TempLeftStr = TempLeftStr & Mid(DispQueueText2(DispQueueHead), (dcCHARSPERLINE/2), 1)
	  TempLeftStr = TempLeftStr & Mid(DispCurrentBottomLine, DispEffectCountT2+1, (dcCHARSPERLINE/2)-DispEffectCountT2)
	  TempRightStr = Mid(DispCurrentBottomLine, (dcCHARSPERLINE/2)+1, (dcCHARSPERLINE/2)-DispEffectCountT2)
	  TempRightStr = TempRightStr & Mid(DispQueueText2(DispQueueHead), (dcCHARSPERLINE/2)+1, 1)
	  TempRightStr = TempRightStr & right(DispCurrentBottomLine, DispEffectCountT2-1)
	  TempBottomStr = TempLeftStr & TempRightStr

	case eBlinkMask
	  TempBottomStr = ""
	  BlinkEffect = True
	  If ((DispEffectCountT2 MOD DispEffectBlinkSlowRate) = 0) Then
		DispEffectBlinkCycleT2 = DispEffectBlinkCycleT2 xor 1
	  End If
	  For i = 1 To dcCHARSPERLINE
		MaskCharacter = Mid(DispQueueText2(DispQueueHead), i, 1)
		If (MaskCharacter <> " ") Then
		  If (DispEffectBlinkCycleT2 = 0) Then
			TempBottomStr = TempBottomStr & MaskCharacter
		  Else
			TempBottomStr = TempBottomStr & " "
		  End If
		Else
		  TempBottomStr = TempBottomStr & Mid(DispCurrentBottomLine, i, 1)
		End If
	  next

	case eBlinkMaskFast
	  TempBottomStr = ""
	  BlinkEffect = True
	  If ((DispEffectCountT2 MOD DispEffectBlinkFastRate) = 0) Then
		DispEffectBlinkCycleT2 = DispEffectBlinkCycleT2 xor 1
	  End If
	  For i = 1 To dcCHARSPERLINE
		MaskCharacter = Mid(DispQueueText2(DispQueueHead), i, 1)
		If (MaskCharacter <> " ") Then
		  If (DispEffectBlinkCycleT2 = 0) Then
			TempBottomStr = TempBottomStr & MaskCharacter
		  Else
			TempBottomStr = TempBottomStr & " "
		  End If
		Else
		  TempBottomStr = TempBottomStr & Mid(DispCurrentBottomLine, i, 1)
		End If
	  next

  End Select

' if the text is "-" Then leave this line alone
  If (DispQueueText2(DispQueueHead) <> "-") Then
	If (Len(TempBottomStr) <> dcCHARSPERLINE) and (Len(TempBottomStr) <> 0) Then
'MsgBox "DISPLAY PROBLEM" & vbNewLine & vbNewLine & "INTERNAL ERROR: STRING TOO LONG" & vbNewLine & "TempBottomStr '" & TempBottomStr & "' Len=" & Len(TempBottomStr), 0, "Black Display Driver Error"
	End If
	DispCurrentBottomLine = TempBottomStr
	TextBoxBottom.Text = TempBottomStr
  End If
		Else
			DispEffectCountT2Dly = DispEffectCountT2Dly - 1
		End If
	End If

' have we run to the end of the effect cycle?
	If (DispEffectCountT1 = DispEffectCountT1End) and (DispEffectCountT2 = DispEffectCountT2End) Then
' leave the effect timer stopped

' if 'TimeOn' = 0 Then this screen never expires, flush the queue
		If (DispQueueTimeOn(DispQueueHead) = 0) Then
			DisplayFlushQueue()
		Else
' start the display timer for 'TimeOn' (the exception to this is 'Blink' effects as they
			' uses TimeOn for the Blink Length)
  If (BlinkEffect = True) Then
	DisplayTimer.Interval  = 10 ' display expires basically immediatly
  Else
	DisplayTimer.Interval  = DispQueueTimeOn(DispQueueHead)
  End If

' and start the timer
			DisplayTimer.Enabled  = True
		End If
	Else
' Else restart the effect timer
		DisplayEffectTimer.Enabled = True
	End If
End Sub

'This function converts the specified number into a fixed length string (9 characters) using
'the following formatting rules ###,###,### (or XXX,YYY,ZZZ).  This allows numbers under
'1 Billion to be formatted correctly for display on the score board
Function FormatScore(ByVal Num)
	Dim Temp
	Dim NumString
	Dim bZero

	NumString = ""						' start off with an empty string
	bZero = False						' and space padding

' handle XXX (001,yyy,zzz - 999,yyy,zzz)
	Temp = int(Num / 1000000)			' if digits in the 1M to 1Bil-1
	If (Temp > 0) Then					' Then convert this to ###,
		NumString = NumString & FormatScore1K(Temp,bZero,True)
		Temp = Temp * 1000000			' remove the 1M portion from the original number
		Num = Num - Temp
		bZero = True					' any numbers converted from now on have to be zero filled
	Else
		NumString = NumString & "   "	' ###
	End If

' handle YYY (xxx,001,zzz - xxx,999,zzz)
	Temp = int(Num / 1000)
	If (Temp > 0) Then
		NumString = NumString & FormatScore1K(Temp,bZero,True)
		Temp = Temp * 1000
		Num = Num - Temp
		bZero = True
	Else
		If (bZero = True) Then
			NumString = NumString & "00À"	' or '###,'
		Else
			NumString = NumString & "   "
		End If
	End If

' convert the last thousand set (ZZZ or (xxx,yyy,001 - xxx,yyy,999))
	NumString = NumString & FormatScore1K(Num, bZero,False)
' return the string to the calling routine
	FormatScore = NumString
End Function

'This function converts the specified number into a fixed length string (6 characters) using
'the following formatting rules ###,###.  This allows the numbers under 1 Million
'to be formatted correctly for display on the score board
Function FormatScore1M(ByVal Num)
	Dim Temp
	Dim NumString
	Dim bZero

	NumString = ""						' start off with an empty string
	bZero = False						' and space padding

' handle XXX (001,yyy - 999,yyy)
	Temp = int(Num / 1000)				' if digits in the 1K to 1M-1
	If (Temp > 0) Then					' Then convert this to ###,
		NumString = NumString & FormatScore1K(Temp,bZero,True)
		Temp = Temp * 1000				' remove the 1K portion from the original number
		Num = Num - Temp
		bZero = True					' any numbers converted from now on have to be zero filled
	Else
		NumString = NumString & "   "	' ###
	End If

' convert the last thousand set (xxx,001 - xxx,999))
	NumString = NumString & FormatScore1K(Num, bZero,False)
' return the string to the calling routine
	FormatScore1M = NumString
End Function

'this function converts a number between 0 and 999 into a 3 byte string which
'is right justified, using either a space or 0 as the passing character
'
'This routine is totally dedicated to my WilliamsPinball font which supports
'the embedded , in the letters and numbers
Function FormatScore1K(Num, bZero, bComma)
	Dim NewNum
	Dim	LastDigit

' get the unit
	LastDigit = Num Mod 10
' remove the unit from the number (just leaves 0 to 99 now)
	NewNum = int(Num / 10)

	If (bZero = True) Then					' do we pad with 0 or space
		If (NewNum >= 10) Then				' padding with 0, convert 10 - 99
			FormatScore1K = NewNum
		Else
			FormatScore1K = "0" & NewNum	' convert 0 - 9
		End If
	Else
		If (NewNum >= 10) Then
			FormatScore1K = NewNum
		Else
			If (NewNum <> 0) Then
				FormatScore1K = " " & NewNum
			Else
				FormatScore1K = "  "
			End If
		End If
	End If
' if we are doing commas, Then convert the last digit
	If (bComma = True) Then
		FormatScore1K = FormatScore1K & Chr(LastDigit + 192)
	Else
		FormatScore1K = FormatScore1K & LastDigit
	End If
End Function

'This function returns a string which is either True or False depending of the state of Bool
Function FormatBool(ByVal Bool)
	If (Bool = True) Then
		FormatBool = "True"
	Else
		FormatBool = "False"
	End If
End Function

rem **************************************************
rem **************************************************
rem		Williams High Score Initals Entry Functions
rem **************************************************
rem **************************************************

Dim hsbModeActive
Dim hsEnteredName
Dim hsEnteredDigits(3)
Dim hsCurrentDigit
Dim hsValidLetters
Dim hsCurrentLetter
Dim hsLetterFlash

Sub HighScoreEntryInit()
	Song="EFMI_HiScore.mp3"
	PlayMusic Song
	hsbModeActive = True
	hsLetterFlash = 0
	hsEnteredDigits(0) = " "	' blank out the name
	hsEnteredDigits(1) = " "
	hsEnteredDigits(2) = " "
	hsCurrentDigit = 0			' start with first digit
	' define the valid characters allowed in the high score name
	hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ'<>*+-/=\^0123456789`"		' ` is back arrow
	' letter display starts with the first character (space)
	hsCurrentLetter = 1
	' flush any other display's queued up and call the display name function
	DisplayFlushQueue()
	HighScoreDisplayNameNow()
	' set the flash rate/update rate of the high score entry
	HighScoreFlashTimer.Interval = 250: HighScoreFlashTimer.Enabled = True
End Sub

rem handle the high score entry keys
Sub HighScoreProcessKey(keycode)
' previous letter
	If keycode = LeftFlipperKey Then
		playsound "oop"
		hsCurrentLetter	= hsCurrentLetter - 1
		If (hsCurrentLetter = 0) Then hsCurrentLetter = len(hsValidLetters)
		HighScoreDisplayNameNow()
	End If
' next letter
	If keycode = RightFlipperKey Then
		playsound "eek"
		hsCurrentLetter	= hsCurrentLetter + 1
		If (hsCurrentLetter > len(hsValidLetters)) Then hsCurrentLetter = 1
		HighScoreDisplayNameNow()
	End If
' previous letter
	If keycode = 2 Then
' if not the backarrow Then commit the letter
		If (mid(hsValidLetters, hsCurrentLetter, 1) <> "`") Then
			playsound "Drain1"
			hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
			hsCurrentDigit = hsCurrentDigit + 1
' if that was the last digit, Then commit the name
			If (hsCurrentDigit = 3) Then
				HighScoreCommitName()
			Else
				HighScoreDisplayNameNow()
			End If
		Else
			playsound "WilliamsEsc"
			hsEnteredDigits(hsCurrentDigit) = " "
			If (hsCurrentDigit > 0) Then hsCurrentDigit = hsCurrentDigit - 1
			HighScoreDisplayNameNow()
		End If
	End If
		If keycode = PlungerKey Then
' if not the backarrow Then commit the letter
		If (mid(hsValidLetters, hsCurrentLetter, 1) <> "`") Then
			playsound "Drain1"
			hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
			hsCurrentDigit = hsCurrentDigit + 1
' if that was the last digit, Then commit the name
			If (hsCurrentDigit = 3) Then
				HighScoreCommitName()
			Else
				HighScoreDisplayNameNow()
			End If
		Else
			playsound "WilliamsEsc"
			hsEnteredDigits(hsCurrentDigit) = " "
			If (hsCurrentDigit > 0) Then hsCurrentDigit = hsCurrentDigit - 1
			HighScoreDisplayNameNow()
		End If
	End If
End Sub

rem display the currently entered initals right now with no flashing letter

Sub HighScoreDisplayNameNow()
' turn off the timer
	HighScoreFlashTimer.Enabled = False
' do an update now (by setting hsLetterFlash to 0 it ensures that the letter is
' displayed and not the flash character "_"
	hsLetterFlash = 0
	HighScoreDisplayName()
' reset the timer (also resets the interval)
	HighScoreFlashTimer.Enabled = True
End Sub

rem Display the Currenly entered name on the screen, with the current letter flashing
rem
rem 	1234567890123456
rem 	ENTER YOUR NAME
rem 	    > AAA <
Sub HighScoreDisplayName()
	Dim i
	Dim TempTopStr
	Dim TempBotStr

' do the top line (simple enough)
	TempTopStr = "ENTER YOUR NAME "
	DispCurrentTopLine = TempTopStr
	TextBoxTop.Text    = TempTopStr

' now do the bottom line (a little more complex)
	TempBotStr = "    > "

' if Then first digit has been entered Then display that
	If (hsCurrentDigit > 0) Then TempBotStr = TempBotStr & hsEnteredDigits(0)
' dito for second digit
	If (hsCurrentDigit > 1) Then TempBotStr = TempBotStr & hsEnteredDigits(1)
' and last digit
	If (hsCurrentDigit > 2) Then TempBotStr = TempBotStr & hsEnteredDigits(2)

' if not all the digits entered Then make the current one flash
	If (hsCurrentDigit <> 3) Then
		If (hsLetterFlash <> 0) Then
			TempBotStr = TempBotStr & "_"
		Else
			TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
		End If
	End If
' pad out the rest of the string if not on the last letter
	If (hsCurrentDigit < 1) Then TempBotStr = TempBotStr & hsEnteredDigits(1)
	If (hsCurrentDigit < 2) Then TempBotStr = TempBotStr & hsEnteredDigits(2)
	TempBotStr = TempBotStr & " <    "
' write it to the display
	DispCurrentBottomLine = TempBotStr
	TextBoxBottom.Text 	  = TempBotStr
End Sub

rem the timer which refreshes the high score entry screen has expired, refreash the display
rem and start again
Sub HighScoreFlashTimer_Timer()
	HighScoreFlashTimer.Enabled = False
	hsLetterFlash = hsLetterFlash + 1
	If (hsLetterFlash = 2) Then hsLetterFlash = 0
	HighScoreDisplayName()
	HighScoreFlashTimer.Enabled = True
End Sub

rem The High Score name has been entered, do what ever the game requires (generally put it in the
rem high score table ;-)
Sub HighScoreCommitName()
	HighScoreFlashTimer.Enabled = False
	hsbModeActive = False
	hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
	If (hsEnteredName = "   ") Then hsEnteredName = "IAN"
	PlaySound "": PlaySound "splash44"
	If (Score > gsHighScore(0)) Then		' have we made the top score
		x=1: MoveScores
' award 2 extra credits if not in free play
		If Freeplay=0 Then
			Credits=Credits+2
			StartLight.State=LightStateOn
			PlaySound "cashregister2"
		End If
	Else
		If (Score > gsHighScore(1)) Then
			x=2: MoveScores
' award 1 extra credit if not in free play
			If Freeplay=0 Then
				Credits=Credits+1
				PlaySound "knocker"
				StartLight.State=LightStateOn
			End If
		Else
			If (Score > gsHighScore(2)) Then
				x=3: MoveScores
			Else
				If (Score > gsHighScore(3)) Then
					x=4: MoveScores
				Else
					If (Score > gsHighScore(4)) Then
						x=5: MoveScores
					End If
				End If
			End If
		End If
	End If
	If (MaxLoopsThisGame > gsLoopChamp) Then
		If Freeplay=0 Then
			Credits=Credits+1
			PlaySound "knocker"
			StartLight.State=LightStateOn
		End If
	End If

' did the player make a high score and/or loop champion
	DisplayFlushQueue()
	DisplayQueueScreen "  *** "&hsEnteredName&" ***  ","   "&FormatScore(Score),8,8,0,1500,False,""
	If (MaxLoopsThisGame > gsLoopChamp) Then
		gsLoopChamp= MaxLoopsThisGame: gsLoopChampName=hsEnteredName
		DisplayQueueScreen "  *** "&hsEnteredName&" ***  "," LOOP  CHAMPION ",3,4,0,1000,False,"thumb"
		DisplayQueueScreen "  *** "&hsEnteredName&" ***  "," WITH "&MaxLoopsThisGame&" LOOPS  ",8,8,0,1000,False,""
	End If
	If Credits=0 Then
		DisplayQueueScreen "     INSERT     ","      COINS     ",3,3,0,1500,False,""
	Else
		DisplayQueueScreen "   CREDITS "&Credits&"    ","  PRESS  START  ",3,3,0,1500,False,""
	End If
	EndMusic(): MusicEndTimer.Enabled=True
	Song="EFMI_Spooky.mp3": PlayMusic Song
	LeftFlipper.RotateToStart: RightFlipper.RotateToStart
	CreditTimer.Enabled=True
	gsLastScore= Score: gsGamesPlayed=gsGamesPlayed+1
	If ReplayGiven=1 Then gsReplayValue=gsReplayValue+50000000
	If ReplayGiven=0 And gsReplayValue>200000000 Then gsReplayValue= gsReplayValue-50000000
	SaveHiScore
End Sub

Sub MoveScores()	'move the scores down and put in the new one
	Dim i
	For i= 4 to x Step -1: gsHighScore(i)=gsHighScore(i-1): gsHighScoreName(i) = gsHighScoreName(i-1): Next
	gsHighScore(x-1)= Score: gsHighScoreName(x-1)=hsEnteredName
End Sub

'************* hurry up stuff ****************

Sub DisplayHurryUp() 'bonus = the time left
	LightArray(50, 1) = 2
	LightArray(31, 1) = 2
	Dim TempBotStr
	Select Case Status
		Case "Normal":			TempBotStr = "   "&FormatScore(HurryUpBonus)&"    "
		Case "Multiball":		TempBotStr = "   "&FormatScore(HurryUpBonus)&"    "
		Case "VoodooMagic":		TempBotStr = " GET THE SCROLL "
		Case "Spitting":		TempBotStr = "  GET THE MUG   "
		Case "HighDive":		TempBotStr = " GET THE PICKER "
		Case "SwordMaster":		TempBotStr = "  GET THE SEAL  "
		Case "MonkeyCombat":	TempBotStr = "  GET THE HAT   "
		Case "DaintyLady":		TempBotStr = "  GET THE MAP   "
		Case "TreasureHunt":	TempBotStr = "  GET THE GOLD  "
		Case "GraveDigger":		TempBotStr = " GET THE CORPSE "
	End Select
	DispCurrentBottomLine = TempBotStr
	DisplayQueueScreen " *HURRY UP* "&HurryUpBonus&"  ",TempBotStr,0,0,0,50,False,""
End Sub

Sub HurryUpTimer_Timer()
	HurryUpTimer.Enabled=False
	HurryUp=1
	DisplayFlushQueue
	DisplayHurryUp
	PlaySound "meterplink"
	HurryUpBonus=HurryUpBonus-1
	If HurryUpBonus=-1 Then
		HurryUp=0
		LightArray(50, 1) = 0
		LightArray(31, 1) = 0
		DisplayFlushQueue
		DisplayQueueScreen "      ITEM      ","      LOST      ",1,2,0,1500,False,"muhaha"
		DisplayQueueScreen "      ITEM      ","      LOST      ",3,3,0,1500,False,""
		HurryUpEndTimer.Enabled=True
	Else
		HurryUpTimer.Enabled=True
	End If
End Sub

Sub HurryUpWaitTimer_Timer()
	HurryUpWaitTimer.Enabled=False
	HurryUpWaitTimer.Interval=3000
	EndMusic()
	HurryUpBonus=Round(300*CoeffTimer)+HurryUpBonus
	If HurryUpBonus > MaxHurryUp Then HurryUpBonus=MaxHurryUp
	HurryUpTimer.Enabled=True
End Sub

Sub HurryUpEndTimer_Timer()
	HurryUpEndTimer.Enabled=False
	DisplayFlushQueue
	GoModeTotal()
End Sub



Sub TiDebug_Timer()
'	Dim BOT
'	TextBox2.Text = CStr(BOT)
'	TextBox2.Text = GIGrog.State
End Sub
