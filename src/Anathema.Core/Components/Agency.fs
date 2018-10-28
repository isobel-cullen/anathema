namespace Anathema.Core.Components

type Characteristic =
| WeaponSkill
| BallisticSkill
| Strength
| Toughness
| Agility
| Intelligence
| Perception
| Willpower
| Fellowship
| Influence

type Characteristics = {
    WeaponSkill : int
    BallisticSkill : int
    Strength : int
    Toughness : int
    Agility : int
    Intelligence : int
    Perception : int
    Willpower : int
    Fellowship : int
    Influence : int
} with
    static member Zero =
        {
            WeaponSkill = 0
            BallisticSkill = 0
            Strength = 0
            Toughness = 0
            Agility = 0
            Intelligence = 0
            Perception = 0
            Willpower = 0
            Fellowship = 0
            Influence = 0
        }

module Characteristics =
    let get scores characteristic =
        match characteristic with
        | WeaponSkill -> scores.WeaponSkill
        | BallisticSkill -> scores.BallisticSkill
        | Strength -> scores.Strength
        | Toughness -> scores.Toughness
        | Agility -> scores.Agility
        | Intelligence -> scores.Intelligence
        | Perception -> scores.Perception
        | Willpower -> scores.Willpower
        | Fellowship -> scores.Fellowship
        | Influence -> scores.Influence

type BehaviourResolver =
| Passive
| Wandering
| Aggressive

type AgencyKind =
| Player
| Npc of BehaviourResolver

type Agency = {
    Energy: int
    Kind: AgencyKind
    Stats: Characteristics
} with
    static member Default = {
        Energy = 0
        Kind = Passive |> Npc
        Stats = Characteristics.Zero
    }

    static member Player =
        { Agency.Default with Kind = Player }

    static member Wandering =
        { Agency.Default with Kind = (Wandering |> Npc) }

    static member Aggressive =
        { Agency.Default with Kind = (Aggressive |> Npc) }
