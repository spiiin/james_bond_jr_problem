open System
open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Diagnostics

module Main =
    let view () = Component(fun ctx ->
        let colorIndicesList = [
            [|1; 2; 2; 1; 3; 4; 4; 3; 3; 4 ;4; 3; 2; 4; 4; 2|];
            [|1; 2; 2; 1; 3; 4; 4; 3; 3; 3; 4; 4; 2; 4; 4; 2|];
            [|2; 2; 1; 1; 3; 4; 4; 3; 3; 3; 4; 4; 2; 4; 4; 2|];
            [|2; 2; 1; 3; 3; 4; 4; 4; 3; 3; 4; 2; 2; 4; 4; 1|];
            [|2; 4; 1; 3; 3; 2; 4; 4; 3; 4; 4; 2; 2; 3; 4; 1|];
            [|4; 1; 3; 2; 3; 2; 4; 4; 3; 4; 4; 2; 2; 3; 4; 1|];
            [|4; 1; 3; 2; 3; 2; 4; 4; 4; 4; 2; 3; 2; 3; 4; 1|];
            [|4; 1; 4; 2; 3; 2; 2; 4; 4; 4; 4; 3; 2; 3; 3; 1|];
            [|4; 3; 4; 2; 3; 1; 2; 4; 4; 2; 4; 3; 2; 4; 3; 1|]
        ]

        let currentIndex = ctx.useState(0)

        let colors = [| Colors.Aqua; Colors.DarkBlue; Colors.White; Colors.Gray |]

        let onNextMove () =
            let nextIndex = (currentIndex.Current + 1) % colorIndicesList.Length
            currentIndex.Set nextIndex

        let onPrevMove () =
            let cur = currentIndex.Current
            currentIndex.Set (if cur > 0 then (cur - 1) else 0)

        StackPanel.create [
            StackPanel.children [
                Button.create [
                    Button.horizontalAlignment HorizontalAlignment.Stretch
                    Button.verticalAlignment VerticalAlignment.Top
                    Button.onClick (fun _ -> onNextMove())
                    Button.content $"NextMove ({currentIndex.Current})"
                    Button.margin (5.0)
                ]
                Button.create [
                    Button.horizontalAlignment HorizontalAlignment.Stretch
                    Button.verticalAlignment VerticalAlignment.Top
                    Button.onClick (fun _ -> onPrevMove())
                    Button.content "PrevMove"
                    Button.margin (5.0)
                ]
                Grid.create [
                    Grid.rowDefinitions "64,64,64,64"
                    Grid.columnDefinitions "64,64,64,64"
                    let indices = colorIndicesList.[currentIndex.Current]
                    Grid.children [
                        for row in 0 .. 3 do
                            for col in 0 .. 3 do
                                let index = indices.[row * 4 + col]
                                let color = colors.[index - 1]
                                Rectangle.create [
                                    Grid.row row
                                    Grid.column col
                                    Rectangle.fill (SolidColorBrush(color))
                                    Rectangle.stroke (SolidColorBrush Colors.Black)
                                    Rectangle.strokeThickness 4.0
                                ]
                    ]
                ]
            ]
        ]
    )

type MainWindow() =
    inherit HostWindow()
    do
        base.Title <- "JamesBondJrSolution"
        base.Content <- Main.view ()
        base.AttachDevTools()

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

[<EntryPoint>]
let main argv =
    AppBuilder
        .Configure<App>()
        .UsePlatformDetect()
        .UseSkia()
        .StartWithClassicDesktopLifetime(argv)
