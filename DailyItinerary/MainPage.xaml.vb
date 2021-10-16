' The Blank Page item template is documented at https://go.microsoft.com/fwlink/?LinkId=402352&clcid=0x409

Imports Windows.ApplicationModel.Background
Imports Windows.Devices.Geolocation
Imports Windows.Storage
Imports Windows.Storage.Streams
Imports Windows.System
''' <summary>
''' An empty page that can be used on its own or navigated to within a Frame.
''' </summary>
Public NotInheritable Class MainPage
    Inherits Page

    Private Async Function WczytajHistoryjke() As Task
        Dim rVal As GeolocationAccessStatus = Await Geolocator.RequestAccessAsync()
        If rVal <> GeolocationAccessStatus.Allowed Then Exit Function

        Await App.WczytajIpolicz()

        Dim dTime As Double = App.miSecs / 3600   ' sekundy -> godziny
        Dim dDistance As Integer = CInt(App.mdMeters / 1000)

        uiDistance.Text = "Daily distance = " & dDistance & " km"
        If dTime > 0 Then
            uiSpeed.Text = "Average speed = " & CInt(dDistance / dTime) & " km/h"
        Else
            uiSpeed.Text = " "
        End If
        uiPoints.Text = "Points: " & App.miPoints & "/" & App.miPointsTotal

        ' 2019.01.24 - blokada gdy nie ma nic do zapisywania
        If App.miPoints = 0 Then
            uiSave.IsEnabled = False
        Else
            uiSave.IsEnabled = True
        End If

        ' Windows.UI.Xaml.Controls.Maps.MapPolyline
    End Function

    'Private Async Sub uiReadHist_Click(sender As Object, e As RoutedEventArgs)
    '    Await WczytajHistoryjke()
    'End Sub

    Private Async Sub uiSave_Click(sender As Object, e As RoutedEventArgs)
        Dim rVal As GeolocationAccessStatus = Await Geolocator.RequestAccessAsync()
        If rVal <> GeolocationAccessStatus.Allowed Then
            DialogBox("No permission for geolocation")
            Exit Sub
        End If

        uiSave.IsEnabled = False
        uiSave.Content = "Saving..."

        Dim iRet As Integer = Await App.SaveGPX(True)
        If iRet = 10 Then
            DialogBox("Cannot write file - no SD card?")
        ElseIf iRet <> 0 Then
            DialogBox("Empty data - file not created")
        Else
            DialogBox("File created")
        End If

        uiSave.Content = "Save history"
        uiSave.IsEnabled = True

    End Sub
    'Private Async Sub RegisterUserTrigger(Optional bOnlyDel As Boolean = False)

    '    For Each oTask In BackgroundTaskRegistration.AllTasks
    '        If oTask.Value.Name = "DailyItinerary_User" Then oTask.Value.Unregister(True)
    '    Next

    '    If bOnlyDel Then Exit Sub

    '    Dim rVal As GeolocationAccessStatus = Await Geolocator.RequestAccessAsync()
    '    If rVal <> GeolocationAccessStatus.Allowed Then Exit Sub

    '    Dim oBAS As BackgroundAccessStatus
    '    oBAS = Await BackgroundExecutionManager.RequestAccessAsync()

    '    If oBAS = BackgroundAccessStatus.AlwaysAllowed Or oBAS = BackgroundAccessStatus.AllowedSubjectToSystemPolicy Then
    '        ' https://docs.microsoft.com/en-us/windows/uwp/launch-resume/create-And-register-an-inproc-background-task
    '        Dim builder As BackgroundTaskBuilder = New BackgroundTaskBuilder
    '        Dim oRet As BackgroundTaskRegistration

    '        builder.SetTrigger(New SystemTrigger(SystemTriggerType.UserPresent, False))
    '        builder.Name = "DailyItinerary_User"
    '        oRet = builder.Register()
    '    End If

    'End Sub
    Private Sub uiLiveTile_Click(sender As Object, e As RoutedEventArgs) Handles uiLiveTile.Click
        SetSettingsBool("liveTile", uiLiveTile.IsChecked)
        If uiLiveTile.IsChecked Then
            ' zarejestruj event
            RegisterUserPresentTrigger()
        Else
            ' wyrejestruj event
            UnregisterTriggers("_userpresent")
        End If
    End Sub

    Private Async Sub uiAutoSave_Click(sender As Object, e As RoutedEventArgs) Handles uiAutoSave.Click
        ' App.SetSettingsBool("autoSave", uiAutoSave.IsChecked)
        If uiAutoSave.IsChecked Then
            ' zarejestruj event
            Await DodajTriggerPolnocny()
        Else
            ' wyrejestruj event
            UnregisterTriggers("_polnocny")
        End If
    End Sub

    Private Async Sub uiRate_Click(sender As Object, e As RoutedEventArgs)
        Dim sUri As Uri = New Uri("ms-windows-store://review/?PFN=" & Package.Current.Id.FamilyName)
        Await Launcher.LaunchUriAsync(sUri)
    End Sub

    Private Async Sub uiPage_GotFocus(sender As Object, e As RoutedEventArgs)
        ' to musi byc z UIthread!
        Dim rVal As GeolocationAccessStatus = Await Geolocator.RequestAccessAsync()
        If rVal <> GeolocationAccessStatus.Allowed Then
            Await App.AddLogLine("uiPage_GotFocus: no allowed geolocation")
            Exit Sub
        End If

        Await WczytajHistoryjke()

        uiLiveTile.IsChecked = GetSettingsBool("liveTile")
        If GetSettingsBool("liveTile") Then
            UnregisterTriggers("_userpresent")
            RegisterUserPresentTrigger()
        End If

        uiAutoSave.IsChecked = IsTriggersRegistered("_polnocny")


        uiVers.Text = "Build " & Package.Current.Id.Version.Major & "." &
            Package.Current.Id.Version.Minor & "." & Package.Current.Id.Version.Build

    End Sub

    Private Async Sub uiAdd_Click(sender As Object, e As RoutedEventArgs) Handles uiAdd.Click

        uiAdd.IsEnabled = False
        uiAdd.Content = "Geolocating..."

        Dim rVal As GeolocationAccessStatus = Await Geolocator.RequestAccessAsync()
        If rVal <> GeolocationAccessStatus.Allowed Then
            DialogBox("Disallowed geolocation?")
            Exit Sub
        End If

        Dim oDevGPS As Geolocator = New Geolocator()

        Dim oPos As Geoposition = Nothing
        oDevGPS.DesiredAccuracyInMeters = 200
        Dim oCacheTime As TimeSpan = New TimeSpan(0, 1, 0)
        Dim oTimeout As TimeSpan = New TimeSpan(0, 0, 15)    ' timeout 

        Dim bError As Boolean = False
        Try
            oPos = Await oDevGPS.GetGeopositionAsync(oCacheTime, oTimeout)
        Catch ex As Exception   ' zapewne timeout
            bError = True
        End Try
        If oPos Is Nothing Then bError = True

        If Not bError AndAlso oPos.Coordinate Is Nothing Then bError = True

        If bError Then
            DialogBox("Error getting (in timely manner) current location")
        Else
            Await WczytajHistoryjke()
        End If

        uiAdd.Content = "Add Point"
        uiAdd.IsEnabled = True
    End Sub

    'Private Sub uiLogReset_Click(sender As Object, e As RoutedEventArgs)
    '    App.msLog = ""
    'End Sub

    'Private Sub uiLog_Show(sender As Object, e As RoutedEventArgs)
    '    App.MsgBox(App.msLog)
    'End Sub
End Class
