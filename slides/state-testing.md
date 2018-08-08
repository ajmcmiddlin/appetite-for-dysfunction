# State machine testing

::: notes
- PBT great for testing referentially transparent functions.
- Most software we right interacts with the murky, stateful world.
- State machine testing builds on PBT to give us a way to test stateful systems
:::

## State machines

## { data-background-image="images/turnstile.png"
      data-background-color="white"
      data-background-size="80%"
      data-background-transition="none"
    }

## { data-background-image="images/turnstile-states.png"
      data-background-color="white"
      data-background-size="80%"
      data-background-transition="none"
    }

## { data-background-image="images/turnstile-initial.png"
      data-background-color="white"
      data-background-size="80%"
      data-background-transition="none"
    }

## { data-background-image="images/turnstile-transitions.png"
      data-background-color="white"
      data-background-size="80%"
      data-background-transition="none"
    }

## State machine testing

- Model application as a state machine
- Generate inputs to transition between states
- Execute inputs against real application
- Update model to reflect expected state changes
- At each step, check no invariants are broken

## Example

<!-- TODO: diagram(s) showing state machine test -->
