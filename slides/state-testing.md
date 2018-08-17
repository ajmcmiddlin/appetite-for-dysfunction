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

## { data-background-image="images/state-example-01.svg"
      class="state-example"
    }

## { data-background-image="images/state-example-02.svg"
      class="state-example"
    }

## { data-background-image="images/state-example-03.svg"
      class="state-example"
    }

## { data-background-image="images/state-example-04.svg"
      class="state-example"
    }

## { data-background-image="images/state-example-05.svg"
      class="state-example"
    }

## { data-background-image="images/state-example-fail-01.svg"
      class="state-example"
    }

## { data-background-image="images/state-example-fail-02.svg"
      class="state-example"
    }

## { data-background-image="images/state-example-fail-03.svg"
      class="state-example"
    }

## WordPress example

##




