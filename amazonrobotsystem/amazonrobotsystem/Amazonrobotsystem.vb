Public Class amazonrobotsystem
    Dim re As Integer = 1
    Dim BMP As Bitmap
    Dim cellvalue(2) As String
    Dim machinen(9) As String
    Dim group() As String = {"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"}
    Sub makeparts(ByVal location As String)
        'make location two part  if location=A7 >>> Cell1 =A   Cell2 =7
        cellvalue(0) = location.Chars(0)
        If location.Length > 2 Then
            cellvalue(1) = location.Chars(1) & location.Chars(2)
        Else
            cellvalue(1) = location.Chars(1)
        End If
        ' cellvalue(1) = location.Remove(0, 0)
    End Sub
    Function def1(ByVal chk As PictureBox)
        Dim re
        'black
        chk.BackgroundImage = Nothing
        ' listmachine()
        If chk.BackgroundImage Is Panel1.BackgroundImage Then chk.BackColor = Color.Red 'عسكري 
        Return re
    End Function
    Function def2(ByVal gtlocation As String)
        Dim re
        Dim pics As New List(Of PictureBox) From {
        a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _
        b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, _
        c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _
        d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, _
        e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, _
        f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, _
        g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, _
        h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, _
        i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, _
        j1, j2, j3, j4, j5, j6, j7, j8, j9, j10, j11, j12, j13, _
        k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, _
        l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13}
        For Each pic In pics

            def1(pic)
            If pic.Name = gtlocation Then
                pic.BackColor = Color.Lime
                re = "Outside :"
                If pic.Name = "c11" Or pic.Name = "c12" Or pic.Name = "d11" Or pic.Name = "d12" Then re = "B1" : pic.BackColor = Color.Green 'B1
                If pic.Name = "f11" Or pic.Name = "f12" Or pic.Name = "g11" Or pic.Name = "g12" Then re = "B2" : pic.BackColor = Color.Green 'B2
                If pic.Name = "i11" Or pic.Name = "i12" Or pic.Name = "j11" Or pic.Name = "j12" Then re = "B3" : pic.BackColor = Color.Green 'B3

                If pic.Name = "c8" Or pic.Name = "c9" Or pic.Name = "d8" Or pic.Name = "d9" Then re = "B4" : pic.BackColor = Color.Orange 'B4
                If pic.Name = "f8" Or pic.Name = "f9" Or pic.Name = "g8" Or pic.Name = "g9" Then re = "B5" : pic.BackColor = Color.Orange 'B5
                If pic.Name = "i8" Or pic.Name = "i9" Or pic.Name = "j8" Or pic.Name = "j9" Then re = "B6" : pic.BackColor = Color.Orange 'B6

                If pic.Name = "c5" Or pic.Name = "c6" Or pic.Name = "d5" Or pic.Name = "d6" Then re = "B7" : pic.BackColor = Color.Yellow 'B7
                If pic.Name = "f5" Or pic.Name = "f6" Or pic.Name = "g5" Or pic.Name = "g6" Then re = "B8" : pic.BackColor = Color.Yellow 'B8
                If pic.Name = "i5" Or pic.Name = "i6" Or pic.Name = "j5" Or pic.Name = "j6" Then re = "B9" : pic.BackColor = Color.Yellow 'B9

                If pic.Name = "c2" Or pic.Name = "c3" Or pic.Name = "d2" Or pic.Name = "d3" Then re = "B10" : pic.BackColor = Color.Cyan 'B10
                If pic.Name = "f2" Or pic.Name = "f3" Or pic.Name = "g2" Or pic.Name = "g3" Then re = "B11" : pic.BackColor = Color.Cyan 'B11
                If pic.Name = "i2" Or pic.Name = "i3" Or pic.Name = "j2" Or pic.Name = "j3" Then re = "B12" : pic.BackColor = Color.Cyan 'B12 

            End If

        Next
        Return re
    End Function
    Sub machine(ByVal location As String)
        '1 to 2
        'white & black
        Dim re As String
        'move 
        makeparts(location)
        'right  left  up  down 
        Dim m, getvalue, lst
        For g = 0 To group.Length - 1
            If group(g) = cellvalue(0) Then
                getvalue = g
            End If
        Next
        For k = 0 To getvalue - 1
            lst = lst & vbCrLf & group(k) & cellvalue(1)
            def2(group(k) & cellvalue(1))

        Next
        For k = getvalue + 1 To group.Length - 1
            lst = lst & vbCrLf & group(k) & cellvalue(1)
            def2(group(k) & cellvalue(1))
        Next
        For i = 1 To 13 - cellvalue(1)
            m = cellvalue(0) & cellvalue(1) + i
            def2(m)
            TextBox3.Text = TextBox3.Text & m & vbCrLf
        Next
        For i = 1 To cellvalue(1) - 1
            m = cellvalue(0) & cellvalue(1) - i
            def2(m)
            TextBox3.Text = TextBox3.Text & m & vbCrLf
        Next
        TextBox3.Text = TextBox3.Text & lst & vbCrLf
        feal(location)
    End Sub
    Sub feal(ByVal location As String)
        makeparts(location)
        'right  left  up  down

        Dim m, getvalue, lst, bn
        For g = 0 To group.Length - 1
            If group(g) = cellvalue(0) Then
                getvalue = g
            End If
        Next
        ' -Up + down
        bn = 0
        For k As Integer = getvalue - 1 To 0 Step -1
            bn += 1
            def2(group(k) & cellvalue(1) + bn)
            lst = lst & group(k) & cellvalue(1) + bn & vbCrLf
        Next
        bn = 0
        For k = getvalue + 1 To group.Length - 1
            bn += 1
            def2(group(k) & cellvalue(1) - bn)
            lst = lst & group(k) & cellvalue(1) - bn & vbCrLf
        Next
        '#####################################################
        ' +Up - down
        bn = 0
        For k As Integer = getvalue - 1 To 0 Step -1
            bn += 1
            def2(group(k) & cellvalue(1) - bn)
            lst = lst & group(k) & cellvalue(1) - bn & vbCrLf
        Next
        bn = 0
        For k = getvalue + 1 To group.Length - 1
            bn += 1
            def2(group(k) & cellvalue(1) + bn)
            lst = lst & group(k) & cellvalue(1) + bn & vbCrLf
        Next
        TextBox3.Text = lst & vbCrLf
        TextBox3.Text = TextBox3.Text.Trim()
        hosan(location)
    End Sub
    Sub hosan(ByVal location As String)
        On Error Resume Next
        makeparts(location)
        'right  left  up  down
        TextBox3.Text = ""
        Dim m, getvalue, lst, bn
        For g = 0 To group.Length - 1
            If group(g) = cellvalue(0) Then
                getvalue = g
            End If
        Next
        bn = 0
        For k = getvalue - 2 To getvalue - 1
            bn += 1
            'If cellvalue(1) - bn > 8 Or cellvalue(1) + bn > 8 Or _
            '    cellvalue(1) - bn < 1 Or cellvalue(1) + bn < 1 Then
            'Else
            def2(group(k) & cellvalue(1) - bn)
            def2(group(k) & cellvalue(1) + bn)
            lst = lst & group(k) & cellvalue(1) - bn & vbCrLf
            lst = lst & group(k) & cellvalue(1) + bn & vbCrLf
            'End If
        Next
        bn = 0
        For k As Integer = getvalue + 2 To getvalue + 1 Step -1
            bn += 1
            'If cellvalue(1) - bn > 8 Or cellvalue(1) + bn > 8 Or _
            '   cellvalue(1) - bn < 1 Or cellvalue(1) + bn < 1 Then
            'Else
            def2(group(k) & cellvalue(1) - bn)
            def2(group(k) & cellvalue(1) + bn)
            lst = lst & group(k) & cellvalue(1) - bn & vbCrLf
            lst = lst & group(k) & cellvalue(1) + bn & vbCrLf
            'End If
        Next k
        TextBox3.Text = lst
        TextBox3.Text = TextBox3.Text.Trim()

    End Sub
    Sub direction()
        BMP = Panel1.BackgroundImage 'Image.FromFile("c:\Users\Public\BFR_ICO.PNG")
        a1.BackgroundImage = Nothing
        If re = 1 Then

            BMP.RotateFlip(RotateFlipType.Rotate90FlipNone)
            a1.BackgroundImage = BMP

        ElseIf re = 2 Then
            BMP.RotateFlip(RotateFlipType.Rotate270FlipNone)
            a1.BackgroundImage = BMP
            re = 1
        End If
        re += 1
    End Sub
    Sub task()
        'start
        'end
    End Sub
    Sub reset()
        a13.BackColor = System.Drawing.SystemColors.AppWorkspace
        b13.BackColor = System.Drawing.Color.AliceBlue
        c13.BackColor = System.Drawing.SystemColors.AppWorkspace
        d13.BackColor = System.Drawing.Color.AliceBlue
        e13.BackColor = System.Drawing.SystemColors.AppWorkspace
        f13.BackColor = System.Drawing.Color.AliceBlue
        g13.BackColor = System.Drawing.SystemColors.AppWorkspace
        h13.BackColor = System.Drawing.Color.AliceBlue
        i13.BackColor = System.Drawing.SystemColors.AppWorkspace
        j13.BackColor = System.Drawing.Color.AliceBlue
        k13.BackColor = System.Drawing.SystemColors.AppWorkspace
        l13.BackColor = System.Drawing.Color.AliceBlue
        l12.BackColor = System.Drawing.SystemColors.AppWorkspace
        k12.BackColor = System.Drawing.Color.AliceBlue
        l11.BackColor = System.Drawing.Color.AliceBlue
        k11.BackColor = System.Drawing.SystemColors.AppWorkspace
        l10.BackColor = System.Drawing.SystemColors.AppWorkspace
        k10.BackColor = System.Drawing.Color.AliceBlue
        l9.BackColor = System.Drawing.Color.AliceBlue
        k9.BackColor = System.Drawing.SystemColors.AppWorkspace
        l8.BackColor = System.Drawing.SystemColors.AppWorkspace
        k8.BackColor = System.Drawing.Color.AliceBlue
        l7.BackColor = System.Drawing.Color.AliceBlue
        k7.BackColor = System.Drawing.SystemColors.AppWorkspace
        l6.BackColor = System.Drawing.SystemColors.AppWorkspace
        k6.BackColor = System.Drawing.Color.AliceBlue
        l5.BackColor = System.Drawing.Color.AliceBlue
        k5.BackColor = System.Drawing.SystemColors.AppWorkspace
        l4.BackColor = System.Drawing.SystemColors.AppWorkspace
        k4.BackColor = System.Drawing.Color.AliceBlue
        l3.BackColor = System.Drawing.Color.AliceBlue
        k3.BackColor = System.Drawing.SystemColors.AppWorkspace
        l2.BackColor = System.Drawing.SystemColors.AppWorkspace
        k2.BackColor = System.Drawing.Color.AliceBlue
        l1.BackColor = System.Drawing.Color.AliceBlue
        k1.BackColor = System.Drawing.SystemColors.ActiveBorder
        j12.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        i12.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        j11.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        i11.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        j10.BackColor = System.Drawing.SystemColors.AppWorkspace
        i10.BackColor = System.Drawing.Color.AliceBlue
        j9.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        i9.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        j8.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        i8.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        j7.BackColor = System.Drawing.Color.AliceBlue
        i7.BackColor = System.Drawing.SystemColors.AppWorkspace
        j6.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        i6.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        j5.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        i5.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        j4.BackColor = System.Drawing.SystemColors.AppWorkspace
        i4.BackColor = System.Drawing.Color.AliceBlue
        j3.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        i3.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        j2.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        i2.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        j1.BackColor = System.Drawing.Color.AliceBlue
        i1.BackColor = System.Drawing.SystemColors.ActiveBorder
        h12.BackColor = System.Drawing.SystemColors.AppWorkspace
        g12.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        f12.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        e12.BackColor = System.Drawing.Color.AliceBlue
        d12.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        c12.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        b12.BackColor = System.Drawing.SystemColors.AppWorkspace
        a12.BackColor = System.Drawing.Color.AliceBlue
        h11.BackColor = System.Drawing.Color.AliceBlue
        g11.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        f11.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        e11.BackColor = System.Drawing.SystemColors.AppWorkspace
        d11.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        c11.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        b11.BackColor = System.Drawing.Color.AliceBlue
        a11.BackColor = System.Drawing.SystemColors.AppWorkspace
        h10.BackColor = System.Drawing.SystemColors.AppWorkspace
        g10.BackColor = System.Drawing.Color.AliceBlue
        f10.BackColor = System.Drawing.SystemColors.AppWorkspace
        e10.BackColor = System.Drawing.Color.AliceBlue
        d10.BackColor = System.Drawing.SystemColors.AppWorkspace
        c10.BackColor = System.Drawing.Color.AliceBlue
        b10.BackColor = System.Drawing.SystemColors.AppWorkspace
        a10.BackColor = System.Drawing.Color.AliceBlue
        h9.BackColor = System.Drawing.Color.AliceBlue
        g9.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        f9.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        e9.BackColor = System.Drawing.SystemColors.AppWorkspace
        d9.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        c9.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        b9.BackColor = System.Drawing.Color.AliceBlue
        a9.BackColor = System.Drawing.SystemColors.AppWorkspace
        h8.BackColor = System.Drawing.SystemColors.AppWorkspace
        g8.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        f8.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        e8.BackColor = System.Drawing.Color.AliceBlue
        d8.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        c8.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        b8.BackColor = System.Drawing.SystemColors.AppWorkspace
        a8.BackColor = System.Drawing.Color.AliceBlue
        h7.BackColor = System.Drawing.Color.AliceBlue
        g7.BackColor = System.Drawing.SystemColors.AppWorkspace
        f7.BackColor = System.Drawing.Color.AliceBlue
        e7.BackColor = System.Drawing.SystemColors.AppWorkspace
        d7.BackColor = System.Drawing.Color.AliceBlue
        c7.BackColor = System.Drawing.SystemColors.AppWorkspace
        b7.BackColor = System.Drawing.Color.AliceBlue
        a7.BackColor = System.Drawing.SystemColors.AppWorkspace
        h6.BackColor = System.Drawing.SystemColors.AppWorkspace
        g6.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        f6.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        e6.BackColor = System.Drawing.Color.AliceBlue
        d6.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        c6.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        b6.BackColor = System.Drawing.SystemColors.AppWorkspace
        a6.BackColor = System.Drawing.Color.AliceBlue
        h5.BackColor = System.Drawing.Color.AliceBlue
        g5.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        f5.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        e5.BackColor = System.Drawing.SystemColors.AppWorkspace
        d5.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        c5.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        b5.BackColor = System.Drawing.Color.AliceBlue
        a5.BackColor = System.Drawing.SystemColors.AppWorkspace
        h4.BackColor = System.Drawing.SystemColors.AppWorkspace
        g4.BackColor = System.Drawing.Color.AliceBlue
        f4.BackColor = System.Drawing.SystemColors.AppWorkspace
        e4.BackColor = System.Drawing.Color.AliceBlue
        d4.BackColor = System.Drawing.SystemColors.AppWorkspace
        c4.BackColor = System.Drawing.Color.AliceBlue
        b4.BackColor = System.Drawing.SystemColors.AppWorkspace
        a4.BackColor = System.Drawing.Color.AliceBlue
        h3.BackColor = System.Drawing.Color.AliceBlue
        g3.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        f3.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        e3.BackColor = System.Drawing.SystemColors.AppWorkspace
        d3.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        c3.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        b3.BackColor = System.Drawing.Color.AliceBlue
        a3.BackColor = System.Drawing.SystemColors.AppWorkspace
        h2.BackColor = System.Drawing.SystemColors.AppWorkspace
        g2.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        f2.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        e2.BackColor = System.Drawing.Color.AliceBlue
        d2.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        c2.BackColor = System.Drawing.Color.FromArgb(224, 224, 224)
        b2.BackColor = System.Drawing.SystemColors.AppWorkspace
        a2.BackColor = System.Drawing.Color.AliceBlue
        h1.BackColor = System.Drawing.Color.AliceBlue
        g1.BackColor = System.Drawing.SystemColors.ActiveBorder
        f1.BackColor = System.Drawing.Color.AliceBlue
        e1.BackColor = System.Drawing.SystemColors.AppWorkspace
        d1.BackColor = System.Drawing.Color.AliceBlue
        c1.BackColor = System.Drawing.SystemColors.AppWorkspace
        b1.BackColor = System.Drawing.Color.AliceBlue
        a1.BackColor = System.Drawing.SystemColors.AppWorkspace

    End Sub
    Sub defbase(ByVal stbas As String, ByVal location As String)
        'B1 , B2 , B3 , B4 , B5 , B6 , B7 , B8 , B9 , B10 , B11 , B12
        'B1  >> c11,c12  ,c11,c12
        'B2  >> f11,f12  ,g11,g12
        'B3  >> i11,i12  ,j11,j12

        'B4  >> c8,c9  ,j8,j9
        'B5  >> f8,f9  ,g8,g9
        'B6  >> i8,i9  ,j8,j9

        'B7  >> c6,c5  ,j6,j5
        'B8  >> f6,f5  ,g6,g5
        'B9  >> i6,i5  ,j6,j5

        'B10  >> c3,c2  ,j3,j2
        'B11  >> f3,f2  ,j3,j2
        'B12  >> i3,i2  ,j3,j2

    End Sub
    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        direction()
        Timer1.Start()
    End Sub
    Dim ty
    Sub cho()
        If ty = ListBox1.Items.Count Then
            ty = 0
        End If
        Label4.Text = ListBox1.Items.Item(ty)
    End Sub
    Private Sub CheckBox1_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles _
        CheckBox1.CheckedChanged, CheckBox2.CheckedChanged, CheckBox3.CheckedChanged, _
        CheckBox4.CheckedChanged, CheckBox5.CheckedChanged, CheckBox6.CheckedChanged, _
        CheckBox7.CheckedChanged, CheckBox8.CheckedChanged, CheckBox9.CheckedChanged
        'If CType(sender, CheckBox).Checked Then
        '    Label1.Text = CType(sender, CheckBox).Text
        'Else
        '    Label1.Text = ""
        'End If
        Dim Check As CheckBox = DirectCast(sender, CheckBox)
        Dim name As String = Check.Name
        ListBox1.Items.Add(Check.Name)
        ty += 1
    End Sub
    Sub listmachine()
        h7.BackgroundImage = Panel1.BackgroundImage
    End Sub
    Private Sub a1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles _
       a1.Click, a2.Click, a3.Click, a4.Click, a5.Click, a6.Click, a7.Click, a8.Click, a9.Click, a10.Click, a11.Click, a12.Click, _
        b1.Click, b2.Click, b3.Click, b4.Click, b5.Click, b6.Click, b7.Click, b8.Click, b9.Click, b10.Click, b11.Click, b12.Click, _
        c1.Click, c2.Click, c3.Click, c4.Click, c5.Click, c6.Click, c7.Click, c8.Click, c9.Click, c10.Click, c11.Click, c12.Click, _
        d1.Click, d2.Click, d3.Click, d4.Click, d5.Click, d6.Click, d7.Click, d8.Click, d9.Click, d10.Click, d11.Click, d12.Click, _
        e1.Click, e2.Click, e3.Click, e4.Click, e5.Click, e6.Click, e7.Click, e8.Click, e9.Click, e10.Click, e11.Click, e12.Click, _
        f1.Click, f2.Click, f3.Click, f4.Click, f5.Click, f6.Click, f7.Click, f8.Click, f9.Click, f10.Click, f11.Click, f12.Click, _
        g1.Click, g2.Click, g3.Click, g4.Click, g5.Click, g6.Click, g7.Click, g8.Click, g9.Click, g10.Click, g11.Click, g12.Click, _
        h1.Click, h2.Click, h3.Click, h4.Click, h5.Click, h6.Click, h7.Click, h8.Click, h9.Click, h10.Click, h11.Click, h12.Click, _
        i1.Click, i2.Click, i3.Click, i4.Click, i5.Click, i6.Click, i7.Click, i8.Click, i9.Click, i10.Click, i11.Click, i12.Click, _
        j1.Click, j2.Click, j3.Click, j4.Click, j5.Click, j6.Click, j7.Click, j8.Click, j9.Click, j10.Click, j11.Click, j12.Click, _
        k1.Click, k2.Click, k3.Click, k4.Click, k5.Click, k6.Click, k7.Click, k8.Click, k9.Click, k10.Click, k11.Click, k12.Click, _
        l1.Click, l2.Click, l3.Click, l4.Click, l5.Click, l6.Click, l7.Click, l8.Click, l9.Click, l10.Click, l11.Click, l12.Click, _
        a13.Click, b13.Click, c13.Click, d13.Click, e13.Click, f13.Click, g13.Click, h13.Click, i13.Click, j13.Click, k13.Click, _
        l13.Click
        reset()
        Dim pic As PictureBox = DirectCast(sender, PictureBox)
        Dim name As String = pic.Name
        machine(name)
        Label74.Text = def2(name) & " " & name
        pic.BackgroundImage = Panel1.BackgroundImage
        h7.BackgroundImage = Panel1.BackgroundImage
    End Sub
    Dim yu
    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        yu += 1
        reset()
        machine("h" & yu)
        For i = 1 To 13
            Dim pis As New List(Of PictureBox) From {h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13}
            For Each pic In pis
                pic.BackgroundImage = Nothing
                If pic.Name = "h" & i Then pic.BackgroundImage = Panel1.BackgroundImage
            Next
        Next
        If yu = 13 Then
            yu = 0
        End If
    End Sub
    Private Sub amazonrobotsystem_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

    End Sub
End Class