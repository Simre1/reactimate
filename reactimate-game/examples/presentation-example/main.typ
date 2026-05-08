#import "@preview/presentate:0.2.5": *
#import "@preview/diagraph:0.3.7": *

#let purple = rgb("#5E5086")
#let lavender = color.lighten(rgb("#8E79C6"), 40%)
#let ink = rgb("#2D2A45")
#let azure = rgb("#00A1F1")
#let teal = rgb("#21C896")
#let offwhite = rgb("#F2F2F7")

#set page(paper: "presentation-16-9", fill: offwhite)
#set text(size: 25pt)
#show raw: set text(font: "FiraCode Nerd Font Mono")
#show math.equation: set text(font: "New Computer Modern Math")

#set page(fill: lavender)

#slide[
  #set align(horizon)

  = Arrowized Functional Reactive Programming
  Signal functions for game loops

  #place(bottom)[
    #box(width: 100%)[

      #place(left)[Reitinger Simon]

      #place(right)[#datetime.today().display()]
    ]
  ]
]

#set page(fill: none)

#show heading: it => box(width: 100%, fill: lavender, inset: 8pt, [
  #it
  #place(right + top)[$lambda$]
])
#set list(marker: text(purple, [•]))
#set page(
  footer-descent: 1em,
  footer: context {
    let val = counter(page).get().first()

    place(left + top, dx: -10%)[
      #box(height: 2em, width: 120%, fill: purple)

    ]
    box(width: 100%, inset: 8pt)[
      #text(fill: white)[
        #place(top + right)[
          #val
        ]
      ]
    ]
  },
)
#set list(spacing: 1.2em)

#slide[
  = Functional Reactive Programming (FRP)

  #set align(center)

  Model values which change over time

  #grid(
    columns: (1fr, 0fr, 1fr),
    row-gutter: 20pt,

    text(weight: "bold")[Classical FRP], [], text(weight: "bold")[Arrowized FRP],
    [Monadic], [], [Arrowized],
  )

  #show: pause

  #grid(
    columns: (1fr, 0fr, 1fr),
    row-gutter: 20pt,

    [$"Time" -> a$], [$"Time"$], [$"Time" -> a -> b$],
    [$10$], [$t_0$], [$lambda a -> a + 10$],
    [$50$], [$t_1$], [$lambda a -> a times 22$],
    [$30$], [$t_2$], [$lambda a -> a - 10$],
  )
]

#slide[
  = Signals

  #stack(
    dir: ltr,
    ```hs
    data Signal a b
    ```,
    h(2cm),
    [Like a function from $a$ to $b$.],
  )

  #show: pause
  #stack(
    [
      ```hs
      (>>>) :: Signal a b -> Signal b c -> Signal a c
      ```
      #show: pause
      ```hs
      (&&&) :: Signal a b -> Signal a c -> Signal a (b,c)
      ```],
    [
      #v(0.5cm)
      #uncover(2, [], update-pause: true)

      #set align(horizon)
      #stack(
        dir: ltr,
        [
          #figure(numbering: none, caption: [```hs >>>```], raw-render(
            ```dot
            digraph {
              rankdir=LR
              f, g
              i, o [shape=none, label=""]
              i -> f [label=a]
              f -> g [label=b]
              g -> o [label=c]
            }
            ```,
          ))],

        [
          #show: pause
          #figure(numbering: none, caption: [```hs &&&```], raw-render(
            ```dot
            digraph {
              rankdir=LR
              f, g
              i, o [shape=none, label=""]
              i -> f [label=a]
              i -> g [label=a]
              f -> o [label=b]
              g -> o [label=c]
            }
            ```,
          ))],
      )],
  )

  #uncover(3, [
    #place(bottom + left)[#text(size: 15pt)[for more details, see `Yampa` package and https://www.haskell.org/arrows]]  
  ])
  
]


#slide[
  = Stateful Signals

  ```hs
  feedback :: b -> Signal (a,b) b -> Signal a b
  ```

  #figure(numbering: none, caption: [```hs feedback```], raw-render(
    ```dot
    digraph {
      rankdir=LR
      f [width=2, height=1]
      i, o, is [shape=none, label=""]
      i -> f [label=a_t]
      f -> o [label=b_t]
      // f -> f [label="b_(t+1)", dir=back]
      is -> f [label="placeholder", style=dashed, arrowhead=vee]
    }
    ```,
    edges: ("is": ("f":[$b_"init"\/b_(t-1)$]))
  ))
]


#slide[
  = Simulation with Signals

  #align(center)[Signals match well with simulation / game loops]

  #show: pause

  ```c
  myCounter = 0
  myGameState = ...

  while (!end) {
    myCounter = myCounter + 10
    myGameState = ...
  }
  ```
]

#slide[
  = Switching

  #align(center)[Compared to monads, arrows have static control flow]
  #show: pause
  #align(center)[Switching allows for dynamic control flow when needed]
   
  #v(1cm)
  
  ```hs
  switch :: Signal a (b, Maybe c) ->
    (c -> Signal a b) -> Signal a b
  ```

  #v(1cm)
  #show: pause

  ```hs
  rSwitch :: Signal a (b, Maybe c) ->
    (c -> Signal a (b, Maybe c)) -> Signal a b
  ```




]

#slide[
  = More things

  - Simple implementation of signal functions
  - Reactimate vs Yampa vs Dunai
  - AFRP and classical FRP
  - AFRP vs ECS for game development
  - Multithreading & Synchronizing different sampling rates
]
