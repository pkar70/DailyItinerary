Imports Windows.ApplicationModel.Background
Imports Windows.Data.Xml.Dom
Imports Windows.Devices.Geolocation
Imports Windows.Storage
Imports Windows.UI.Notifications
Imports Windows.UI.Popups
''' <summary>
''' Provides application-specific behavior to supplement the default Application class.
''' </summary>

NotInheritable Class App
    Inherits Application

    Public Shared miSecs As Integer = 0
    Public Shared mdMeters As Double = 0
    Public Shared miPoints As Integer = 0
    Public Shared miPointsTotal As Integer = 0

    Public Shared msLog As String = ""
    Private Shared olGeoHist As List(Of Geocoordinate) = New List(Of Geocoordinate)
#Region "ByWizard"

    Protected Function OnLaunchFragment(aes As ApplicationExecutionState) As Frame
        Dim mRootFrame As Frame = TryCast(Window.Current.Content, Frame)

        ' Do not repeat app initialization when the Window already has content,
        ' just ensure that the window is active

        If mRootFrame Is Nothing Then
            ' Create a Frame to act as the navigation context and navigate to the first page
            mRootFrame = New Frame()

            AddHandler mRootFrame.NavigationFailed, AddressOf OnNavigationFailed

            ' PKAR added wedle https://stackoverflow.com/questions/39262926/uwp-hardware-back-press-work-correctly-in-mobile-but-error-with-pc
            AddHandler mRootFrame.Navigated, AddressOf OnNavigatedAddBackButton
            AddHandler Windows.UI.Core.SystemNavigationManager.GetForCurrentView().BackRequested, AddressOf OnBackButtonPressed

            ' Place the frame in the current Window
            Window.Current.Content = mRootFrame
        End If

        Return mRootFrame
    End Function

    ''' <summary>
    ''' Invoked when the application is launched normally by the end user.  Other entry points
    ''' will be used when the application is launched to open a specific file, to display
    ''' search results, and so forth.
    ''' </summary>
    ''' <param name="e">Details about the launch request and process.</param>
    Protected Overrides Sub OnLaunched(e As Windows.ApplicationModel.Activation.LaunchActivatedEventArgs)
        Dim rootFrame As Frame = OnLaunchFragment(e.PreviousExecutionState)

        If e.PrelaunchActivated = False Then
            If rootFrame.Content Is Nothing Then
                ' When the navigation stack isn't restored navigate to the first page,
                ' configuring the new page by passing required information as a navigation
                ' parameter
                rootFrame.Navigate(GetType(MainPage), e.Arguments)
            End If

            ' Ensure the current window is active
            Window.Current.Activate()
        End If
    End Sub

    ' CommandLine, Toasts
    Protected Overrides Async Sub OnActivated(args As IActivatedEventArgs)
        ' to jest m.in. dla Toast i tak dalej?

        ' próba czy to commandline
        If args.Kind = ActivationKind.CommandLineLaunch Then

            Dim commandLine As CommandLineActivatedEventArgs = TryCast(args, CommandLineActivatedEventArgs)
            Dim operation As CommandLineActivationOperation = commandLine?.Operation
            Dim strArgs As String = operation?.Arguments

            If Not String.IsNullOrEmpty(strArgs) Then
                Await ObsluzCommandLine(strArgs)
                Window.Current.Close()
                Return
            End If
        End If

        ' jesli nie cmdline (a np. toast), albo cmdline bez parametrow, to pokazujemy okno
        Dim rootFrame As Frame = OnLaunchFragment(args.PreviousExecutionState)

        rootFrame.Navigate(GetType(MainPage))

        Window.Current.Activate()

    End Sub


    ''' <summary>
    ''' Invoked when Navigation to a certain page fails
    ''' </summary>
    ''' <param name="sender">The Frame which failed navigation</param>
    ''' <param name="e">Details about the navigation failure</param>
    Private Sub OnNavigationFailed(sender As Object, e As NavigationFailedEventArgs)
        Throw New Exception("Failed to load Page " + e.SourcePageType.FullName)
    End Sub

    ''' <summary>
    ''' Invoked when application execution is being suspended.  Application state is saved
    ''' without knowing whether the application will be terminated or resumed with the contents
    ''' of memory still intact.
    ''' </summary>
    ''' <param name="sender">The source of the suspend request.</param>
    ''' <param name="e">Details about the suspend request.</param>
    Private Sub OnSuspending(sender As Object, e As SuspendingEventArgs) Handles Me.Suspending
        Dim deferral As SuspendingDeferral = e.SuspendingOperation.GetDeferral()
        ' TODO: Save application state and stop any background activity
        deferral.Complete()
    End Sub
#End Region

#Region "Settings"

    'Public Shared Function GetSettingsBool(sName As String, Optional iDefault As Boolean = False) As Boolean
    '    Dim sTmp As Boolean

    '    sTmp = iDefault

    '    If ApplicationData.Current.RoamingSettings.Values.ContainsKey(sName) Then
    '        sTmp = CBool(ApplicationData.Current.RoamingSettings.Values(sName).ToString)
    '    End If
    '    If ApplicationData.Current.LocalSettings.Values.ContainsKey(sName) Then
    '        sTmp = CBool(ApplicationData.Current.LocalSettings.Values(sName).ToString)
    '    End If

    '    Return sTmp

    'End Function
    'Public Shared Sub SetSettingsBool(sName As String, sValue As Boolean, Optional bRoam As Boolean = False)
    '    If bRoam Then ApplicationData.Current.RoamingSettings.Values(sName) = sValue.ToString
    '    ApplicationData.Current.LocalSettings.Values(sName) = sValue.ToString
    'End Sub
#End Region

    'Public Shared Function GPSdistanceDwa(dLat0 As Double, dLon0 As Double, dLat As Double, dLon As Double) As Integer
    '    ' https://stackoverflow.com/questions/28569246/how-to-get-distance-between-two-locations-in-windows-phone-8-1

    '    Dim iRadix As Integer = 6371000
    '    Dim tLat As Double = (dLat - dLat0) * Math.PI / 180
    '    Dim tLon As Double = (dLon - dLon0) * Math.PI / 180
    '    Dim a As Double = Math.Sin(tLat / 2) * Math.Sin(tLat / 2) +
    '        Math.Cos(Math.PI / 180 * dLat0) * Math.Cos(Math.PI / 180 * dLat) *
    '        Math.Sin(tLon / 2) * Math.Sin(tLon / 2)
    '    Dim c As Double = 2 * Math.Asin(Math.Min(1, Math.Sqrt(a)))
    '    Dim d As Double = iRadix * c

    '    Return d

    'End Function
    'Private Shared Function GPSdistance(ByRef oPos1 As Geocoordinate, ByRef oPos2 As Geocoordinate) As Double
    '    Return GPSdistanceDwa(oPos1.Point.Position.Latitude, oPos1.Point.Position.Longitude, oPos2.Point.Position.Latitude, oPos2.Point.Position.Longitude)
    'End Function

    'Private Shared Async Function GetLogDir() As Task(Of StorageFolder)
    '    Try

    '        Dim externalDevices As StorageFolder = KnownFolders.RemovableDevices
    '        Dim oCards As IReadOnlyList(Of StorageFolder) = Await externalDevices.GetFoldersAsync()
    '        Dim sdCard As StorageFolder = oCards.FirstOrDefault()
    '        If sdCard Is Nothing Then Return Nothing

    '        Dim oFolder As StorageFolder = Await sdCard.CreateFolderAsync("DataLogs", CreationCollisionOption.OpenIfExists)
    '        If oFolder IsNot Nothing Then Return Await oFolder.CreateFolderAsync("DailyItinerary", CreationCollisionOption.OpenIfExists)

    '    Catch ex As Exception
    '        Return Nothing
    '    End Try

    '    Return Nothing

    'End Function


    Private Shared Async Function GetAnyFile(sFile As String) As Task(Of StorageFile)
        Dim oFold As StorageFolder
        oFold = Await GetLogFolderRootAsync(False)
        If oFold Is Nothing Then Return Nothing

        Try
            Return Await oFold.CreateFileAsync(sFile, CreationCollisionOption.OpenIfExists)
        Catch ex As Exception
            Return Nothing
        End Try

    End Function
    Private Shared Async Function GetLogFile() As Task(Of StorageFile)
        Return Await GetAnyFile("_DailyItinerary-log.txt")
    End Function
    Private Shared Async Function GetStatFile() As Task(Of StorageFile)
        Return Await GetAnyFile("_DailyItinerary-statistic.txt")
    End Function

    Public Shared Async Function AddLogLine(sMsg As String) As Task

        'msLog = msLog & Date.Now & " " & sMsg & vbCrLf
        'Dim oFile = Await GetLogFile()
        'If oFile Is Nothing Then Exit Function

        'Dim sError = ""

        'If False Then
        '    ' zapis do logu uzywajac strumieni
        '    Try
        '        Dim oStream = Await oFile.OpenStreamForWriteAsync()
        '        oStream.Seek(0, SeekOrigin.End)
        '        Dim oWrtr = New StreamWriter(oStream)
        '        oWrtr.WriteLine(Date.Now & " " & sMsg)
        '        oWrtr.Dispose()
        '        oStream.Dispose()
        '    Catch ex As Exception
        '        sError = ex.Message
        '    End Try
        'Else
        '    ' prostszy zapis do pliku
        '    Try
        '        Await FileIO.AppendTextAsync(oFile, Date.Now & " " & sMsg & vbCrLf)
        '    Catch ex As Exception
        '        sError = ex.Message
        '    End Try

        '    ' If sError <> "" Then MsgBox("ERROR adding to LOG file: " & sError)

        'End If

    End Function
    Public Shared Async Function AddStatLine(sMsg As String) As Task
        Dim oFile As StorageFile = Await GetStatFile()
        If oFile Is Nothing Then Exit Function

        Dim sError As String = ""

        If False Then
            ' zapis do logu uzywajac strumieni
            Try
                Dim oStream As Stream = Await oFile.OpenStreamForWriteAsync()
                oStream.Seek(0, SeekOrigin.End)
                Dim oWrtr As StreamWriter = New StreamWriter(oStream)
                oWrtr.WriteLine(sMsg)
                oWrtr.Dispose()
                oStream.Dispose()
            Catch ex As Exception
                sError = ex.Message
            End Try
        Else
            ' prostszy zapis do pliku
            Try
                Await FileIO.AppendTextAsync(oFile, sMsg & vbCrLf)
            Catch ex As Exception
                sError = ex.Message
            End Try

            ' If sError <> "" Then MsgBox("ERROR adding to LOG file: " & sError)

        End If

    End Function
    'Public Shared Async Sub MsgBox(sMsg As String)
    '    Dim oMsg As MessageDialog = New MessageDialog(sMsg)
    '    Await oMsg.ShowAsync
    'End Sub

    Private Shared Async Function SkopiujHistorie() As Task
        Dim oDate As DateTimeOffset

        ' sprawdzenie ostatniego dodanego we wlasnej historii
        If olGeoHist.Count < 1 Then
            oDate = Date.Now.AddHours(-25)  ' czyli idziemy po maksymalnym zasiegu gdy nie mamy nic
        Else
            oDate = olGeoHist.ElementAt(olGeoHist.Count - 1).Timestamp.AddSeconds(1)
        End If


        Dim oPosHist As IReadOnlyList(Of Geoposition) = Nothing
        Dim sError As String = ""
        Try
            oPosHist = Await Geolocator.GetGeopositionHistoryAsync(oDate)
        Catch ex As Exception
            sError = ex.Message
        End Try
        If sError <> "" Then
            Await AddLogLine("Kopiuj historie: ERROR getting history: " & sError)
            Exit Function
        End If

        If oPosHist Is Nothing Then
            Await AddLogLine("Kopiuj historie: oPosHist is nothing?")
            Exit Function
        End If

        For i As Integer = 0 To oPosHist.Count - 1
            olGeoHist.Add(oPosHist.ElementAt(i).Coordinate)
        Next

    End Function

    Private Shared Sub CountStat()
        mdMeters = 0
        miSecs = 0

        miPoints = 0
        miPointsTotal = olGeoHist.Count

        Dim iDay As Integer = Date.Now.Day

        For i As Integer = 1 To olGeoHist.Count - 1
            If olGeoHist.ElementAt(i).Timestamp.Day = iDay Then
                ' Dim dMetr As Double = GPSdistance(olGeoHist.ElementAt(i - 1), olGeoHist.ElementAt(i))
                Dim dMetr As Double = olGeoHist.ElementAt(i - 1).DistanceTo(olGeoHist.ElementAt(i))
                Dim dSec As Double = (olGeoHist.ElementAt(i).Timestamp - olGeoHist.ElementAt(i - 1).Timestamp).TotalSeconds
                Dim dAccu As Double = olGeoHist.ElementAt(i).Accuracy + olGeoHist.ElementAt(i - 1).Accuracy
                ' If dMetr / dSec > 500 / 3600 Then
                If dMetr > dAccu Then
                    mdMeters += dMetr
                    miSecs += dSec
                    miPoints += 1
                End If
            End If
        Next

    End Sub

    'Private Shared Function IsDateChanged() As Boolean

    '    Dim iDay0 As Integer = olGeoHist.ElementAt(0).Timestamp.Day

    '    For i As Integer = 1 To olGeoHist.Count - 1
    '        If olGeoHist.ElementAt(i).Timestamp.Day <> iDay0 Then Return True
    '    Next

    '    Return False
    'End Function


    Private Sub LiveTileUpdate()

        Dim sDistance As String
        Dim iKm As Integer = mdMeters / 1000

        If iKm < 100 Then
            sDistance = iKm.ToString("####0.0")
        Else
            sDistance = iKm.ToString("####0")
        End If

        Dim sTmp As String
        sTmp = "<tile><visual>"
        sTmp = sTmp & "<binding template='TileSmall' branding='none' hint-textStacking='center' >"
        sTmp = sTmp & "<text hint-style='title' hint-align='center'>" & sDistance & "</text>"
        sTmp = sTmp & "<text hint-style='caption' hint-align='center'>km</text>"
        sTmp = sTmp & "</binding>"

        sTmp = sTmp & "<binding template='TileMedium' hint-textStacking='center'>"
        sTmp = sTmp & "<text hint-style='caption' hint-align='center'>km</text>"
        sTmp = sTmp & "</binding>"

        sTmp = sTmp & "</visual></tile>"

        Dim oXml As XmlDocument = New XmlDocument
        oXml.LoadXml(sTmp)

        Dim oTUPS As TileUpdater = TileUpdateManager.CreateTileUpdaterForApplication
        Dim oTile As TileNotification = New Windows.UI.Notifications.TileNotification(oXml)
        oTUPS.Update(oTile)

    End Sub

    Public Shared Async Function SaveGPX(bForce As Boolean) As Task(Of Integer)

        ' gdy nie ma listy, to nic do zapisywania...
        If olGeoHist.Count < 1 Then Return 1

        Dim dDay0 As DateTimeOffset = olGeoHist.ElementAt(0).Timestamp
        ' gdy lista nie siega poprzedniego dnia - nie zapisuj
        If dDay0.Day = Date.Now.Day Then If Not bForce Then Return 2

        ' *TODO*: uwzglednienie serii dni, a nie tylko jednego - czyli ze kilka plikow

        ' https://en.wikipedia.org/wiki/GPS_Exchange_Format
        Dim sGpx As String
        sGpx = "<gpx version='1.1' creator='Daily Itinerary'> " & vbCrLf
        sGpx &= "<metadata>" & vbCrLf
        sGpx &= "<desc>Daily itinerary for " & dDay0.ToString("yyyy.MM.dd") & "</desc>" & vbCrLf
        sGpx &= "<time>" & dDay0.ToString("yyyyMMdd") & "</time>" & vbCrLf
        sGpx &= "</metadata>" & vbCrLf

        sGpx &= "<trk>" & vbCrLf
        sGpx &= "<name>Whole day</name>" & vbCrLf
        sGpx &= "<trkseg>" & vbCrLf
        For i As Integer = 0 To olGeoHist.Count - 1
            If olGeoHist.ElementAt(i).Timestamp.Day <> dDay0.Day Then Exit For

            ' tu moze byc weryfikacja zrodla
            ' If olGeoHist.ElementAt(i).PositionSource = ' = PositionSource.Satellite (WiFi, Cellular, IPaddress, Unknown; Default, Obfuscated


            sGpx &= "<trkpt "
            sGpx &= "lat='" & olGeoHist.ElementAt(i).Point.Position.Latitude & "' "
            sGpx &= "lon='" & olGeoHist.ElementAt(i).Point.Position.Longitude & "' >" & vbCrLf
            If olGeoHist.ElementAt(i).Point.Position.Altitude <> 0 Then
                sGpx &= "<ele>" & olGeoHist.ElementAt(i).Point.Position.Altitude & "</ele>" & vbCrLf
            End If
            sGpx &= "<time>" & olGeoHist.ElementAt(i).Timestamp.ToString("yyyy-MM-ddTHH:mm:ssZ") & "</time>" & vbCrLf
            sGpx &= "</trkpt>" & vbCrLf
        Next

        sGpx &= "</trkseg>" & vbCrLf
        sGpx &= "</trk>" & vbCrLf
        sGpx &= "</gpx>" & vbCrLf

        Dim oFold As StorageFolder = Await GetLogFolderMonthAsync(False)
        'Dim oFold As StorageFolder = Await GetLogDir()
        'If oFold Is Nothing Then Return 10
        'Dim oFold1 As StorageFolder = Await oFold.CreateFolderAsync(dDay0.ToString("yyyy"), CreationCollisionOption.OpenIfExists)
        'If oFold1 IsNot Nothing Then
        '    oFold = oFold1 ' jak sie nie udalo z katalogiem roku, to zapisz do glownego
        '    oFold1 = Await oFold.CreateFolderAsync(dDay0.ToString("MM"), CreationCollisionOption.OpenIfExists)
        '    If oFold1 IsNot Nothing Then oFold = oFold1 ' jak sie nie udalo z katalogiem miesiaca, to zapisz do roku/glownego
        'End If

        Dim sFile As String
        If bForce Then
            sFile = dDay0.ToString("yyyy.MM.dd.HH.mm") & ".gpx"
        Else
            sFile = dDay0.ToString("yyyy.MM.dd") & ".gpx"
        End If
        Dim oFile As StorageFile = Await oFold.CreateFileAsync(sFile, CreationCollisionOption.OpenIfExists)
        Await FileIO.WriteTextAsync(oFile, sGpx)
        Return 0
    End Function

    Private Async Function WriteStat() As Task
        Dim sTmp As String = Date.Now.ToString("yyyy.MM.dd.HH.mm") & " " & mdMeters & " meters in " &
            miSecs & " seconds, i.e. " & ((mdMeters / 1000) / (miSecs / 3600)).ToString("####0.000") & " km/h " &
            "(from " & miPoints & "/" & miPointsTotal & " points)"

        Await AddStatLine(sTmp)
        'sTmp = Date.Now.ToString("yyyy.MM.dd.HH.mm") & " to juz po zapisie"
        'Await AddStatLine(sTmp)

    End Function

    Private Async Function TryWriteHist() As Task
        Dim bFull As Boolean = False
        Dim oDir As StorageFolder = Await GetLogFolderRootAsync()
        If oDir Is Nothing Then Exit Function

        'Dim sFile As String = "pkarmode.txt"
        'Dim oFile As StorageFile = Await oDir.TryGetItemAsync(sFile)
        'If oFile Is Nothing Then Exit Function

        Await SaveGPX(False)

    End Function

    Private Sub DelPrevDaysFromList()
        ' gdy nie ma listy, to nic do kasowania tym bardziej
        If olGeoHist.Count < 1 Then Exit Sub

        Dim iDay As Integer = Date.Now.Day
        Dim iInd As Integer
        For iInd = 0 To olGeoHist.Count     ' As jest wyzej, bo chce miec po petli dostep do iInd
            If olGeoHist.ElementAt(iInd).Timestamp.Day = iDay Then Exit For
        Next

        If iInd < 1 Then Exit Sub

        olGeoHist.RemoveRange(0, iInd)
    End Sub

    Public Shared Async Function WczytajIpolicz() As Task
        Await SkopiujHistorie()
        CountStat()
    End Function


#If False Then
    Public Shared Async Function DodajTriggerPolnocny() As Task

        Await AddLogLine("DodajTriggerPolnocny START")

        Dim oBAS As BackgroundAccessStatus
        oBAS = Await BackgroundExecutionManager.RequestAccessAsync()
        Await AddLogLine("DTP got oBAS")

        ' to musi byc z UIthread!
        'Dim rVal As GeolocationAccessStatus = Await Geolocator.RequestAccessAsync()
        'If rVal <> GeolocationAccessStatus.Allowed Then
        '    Await AddLogLine("DTP error: no allowed geolocation")
        '    Exit Function
        'End If

        If oBAS = BackgroundAccessStatus.AlwaysAllowed Or oBAS = BackgroundAccessStatus.AllowedSubjectToSystemPolicy Then
            '    ' https://docs.microsoft.com/en-us/windows/uwp/launch-resume/create-And-register-an-inproc-background-task

            For Each oTask As ... In BackgroundTaskRegistration.AllTasks
                If oTask.Value.Name = "DailyItinerary_Daily" Then
                    oTask.Value.Unregister(True)
                    Await AddLogLine("DTP: old trigger deleted")
                End If
            Next

            Dim builder As BackgroundTaskBuilder = New BackgroundTaskBuilder
            Dim oRet As BackgroundTaskRegistration

            Dim iMin As integer = (24 * 60) - 20    ' 24 godziny po 60 minut bez 20 minut; czyli czas uruchomienia
            iMin -= Date.Now.Hour() * 60  ' odjÄ…Ä‡ aktualny czas
            iMin -= Date.Now.Minute()

            builder.SetTrigger(New TimeTrigger(iMin, True))
            builder.Name = "DailyItinerary_Daily"
            oRet = builder.Register()
            Await AddLogLine("DTP, task registered, iMin=" & iMin.ToString)
        Else
            Await AddLogLine("DTP error: no oBAS")
        End If


    End Function
#End If

    ' Private moTimerDeferal As BackgroundTaskDeferral = Nothing

    'Protected Overrides Async Sub OnBackgroundActivated(args As BackgroundActivatedEventArgs)
    '    '        If moTimerDeferal IsNot Nothing Then Exit Sub   ' juz jeden jest, choc nie powinien
    '    moTimerDeferal = args.TaskInstance.GetDeferral()

    '    Await SkopiujHistorie()
    '    CountStat()
    '    If args.TaskInstance.Task.Name = "DailyItinerary_User" Then LiveTileUpdate()


    '    If IsDateChanged() Then
    '        If GetSettingsBool("autoSave") Then
    '            Await WriteStat()
    '            Await TryWriteHist()
    '        End If
    '        DelPrevDaysFromList()
    '    End If

    '    If args.TaskInstance.Task.Name = "DailyItinerary_Daily" Then Await DodajTriggerPolnocny()

    '    moTimerDeferal.Complete()

    'End Sub

    'Public Shared Async Function DialogBox(sMsg As String) As Task
    '    Dim oMsg As Windows.UI.Popups.MessageDialog = New Windows.UI.Popups.MessageDialog(sMsg)
    '    Await oMsg.ShowAsync
    'End Function

    Private Async Function AppServiceLocalCommand(sCommand As String) As Task(Of String)
        Return ""
    End Function

    ' RemoteSystems, Timer
    Protected Overrides Async Sub OnBackgroundActivated(args As BackgroundActivatedEventArgs)
        moTaskDeferal = args.TaskInstance.GetDeferral() ' w pkarmodule.App

        Dim bNoComplete As Boolean = False
        Dim bObsluzone As Boolean = False

        If IsThisTriggerPolnocny(args) OrElse args.TaskInstance.Task.Name.Contains("userpresent") Then
            Await SkopiujHistorie()
            CountStat()

            If args.TaskInstance.Task.Name.Contains("userpresent") Then
                LiveTileUpdate()
            Else
                If GetSettingsBool("autoSave") Then
                    Await WriteStat()
                    Await TryWriteHist()
                End If
                DelPrevDaysFromList()
            End If

            bObsluzone = True
        End If

        ' lista komend danej aplikacji
        Dim sLocalCmds As String = ""

        ' zwroci false gdy to nie jest RemoteSystem; gdy true, to zainicjalizowało odbieranie
        If Not bObsluzone Then bNoComplete = RemSysInit(args, sLocalCmds)

        If Not bNoComplete Then moTaskDeferal.Complete()

    End Sub

End Class
