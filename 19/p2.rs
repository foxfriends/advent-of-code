use std::collections::{BTreeMap, HashSet};
use std::io::stdin;
use std::str::FromStr;

#[derive(Copy, Clone, Debug)]
struct Blueprint {
    id: u32,
    ore: u32,
    clay: u32,
    obsidian: (u32, u32),
    geode: (u32, u32),
}

impl FromStr for Blueprint {
    type Err = <u32 as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut words = s.split(" ");
        let id = words.nth(1).unwrap();
        let id = &id[0..id.len() - 1];

        let ore = words.nth(4).unwrap();
        let clay = words.nth(5).unwrap();
        let obsidian_ore = words.nth(5).unwrap();
        let obsidian_clay = words.nth(2).unwrap();
        let geode_ore = words.nth(5).unwrap();
        let geode_obsidian = words.nth(2).unwrap();

        Ok(Self {
            id: id.parse()?,
            ore: ore.parse()?,
            clay: clay.parse()?,
            obsidian: (obsidian_ore.parse()?, obsidian_clay.parse()?),
            geode: (geode_ore.parse()?, geode_obsidian.parse()?),
        })
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
struct State {
    time_left: u32,
    ore_bot: u32,
    ore: u32,
    clay_bot: u32,
    clay: u32,
    obsidian_bot: u32,
    obsidian: u32,
    geode: u32,
}

impl State {
    fn next_build_ore(&self, blueprint: &Blueprint) -> Option<Self> {
        let most_expensive = u32::max(
            u32::max(blueprint.ore, blueprint.clay),
            u32::max(blueprint.obsidian.0, blueprint.geode.0),
        );
        if self.ore_bot * self.time_left + self.ore >= most_expensive * self.time_left {
            return None;
        }
        let ore_needed = blueprint.ore.saturating_sub(self.ore);
        let time_needed =
            1 + ore_needed / self.ore_bot + if ore_needed % self.ore_bot > 0 { 1 } else { 0 };
        if time_needed >= self.time_left {
            return None;
        }
        Some(Self {
            time_left: self.time_left - time_needed,
            ore_bot: self.ore_bot + 1,
            ore: self.ore + self.ore_bot * time_needed - blueprint.ore,
            clay: self.clay + self.clay_bot * time_needed,
            obsidian: self.obsidian + self.obsidian_bot * time_needed,
            ..*self
        })
    }

    fn next_build_clay(&self, blueprint: &Blueprint) -> Option<Self> {
        if self.clay_bot * self.time_left + self.clay >= blueprint.obsidian.1 * self.time_left {
            return None;
        }
        let ore_needed = blueprint.clay.saturating_sub(self.ore);
        let time_needed =
            1 + ore_needed / self.ore_bot + if ore_needed % self.ore_bot > 0 { 1 } else { 0 };
        if time_needed >= self.time_left {
            return None;
        }
        Some(Self {
            time_left: self.time_left - time_needed,
            clay_bot: self.clay_bot + 1,
            ore: self.ore + self.ore_bot * time_needed - blueprint.clay,
            clay: self.clay + self.clay_bot * time_needed,
            obsidian: self.obsidian + self.obsidian_bot * time_needed,
            ..*self
        })
    }

    fn next_build_obsidian(&self, blueprint: &Blueprint) -> Option<Self> {
        if self.clay_bot == 0 {
            return None;
        }
        if self.obsidian_bot * self.time_left + self.obsidian >= blueprint.geode.1 * self.time_left
        {
            return None;
        }
        let ore_needed = blueprint.obsidian.0.saturating_sub(self.ore);
        let clay_needed = blueprint.obsidian.1.saturating_sub(self.clay);
        let time_needed = u32::max(
            1 + ore_needed / self.ore_bot + if ore_needed % self.ore_bot > 0 { 1 } else { 0 },
            1 + clay_needed / self.clay_bot
                + if clay_needed % self.clay_bot > 0 {
                    1
                } else {
                    0
                },
        );
        if time_needed >= self.time_left {
            return None;
        }
        Some(Self {
            time_left: self.time_left - time_needed,
            obsidian_bot: self.obsidian_bot + 1,
            ore: self.ore + self.ore_bot * time_needed - blueprint.obsidian.0,
            clay: self.clay + self.clay_bot * time_needed - blueprint.obsidian.1,
            obsidian: self.obsidian + self.obsidian_bot * time_needed,
            ..*self
        })
    }

    fn next_build_geode(&self, blueprint: &Blueprint) -> Option<Self> {
        if self.obsidian_bot == 0 {
            return None;
        }
        let ore_needed = blueprint.geode.0.saturating_sub(self.ore);
        let obsidian_needed = blueprint.geode.1.saturating_sub(self.obsidian);
        let time_needed = u32::max(
            1 + ore_needed / self.ore_bot + if ore_needed % self.ore_bot > 0 { 1 } else { 0 },
            1 + obsidian_needed / self.obsidian_bot
                + if obsidian_needed % self.obsidian_bot > 0 {
                    1
                } else {
                    0
                },
        );
        if time_needed >= self.time_left {
            return None;
        }
        Some(Self {
            time_left: self.time_left - time_needed,
            geode: self.geode + self.time_left - time_needed,
            ore: self.ore + self.ore_bot * time_needed - blueprint.geode.0,
            clay: self.clay + self.clay_bot * time_needed,
            obsidian: self.obsidian + self.obsidian_bot * time_needed - blueprint.geode.1,
            ..*self
        })
    }

    fn next_state(&self, blueprint: &Blueprint) -> Vec<Self> {
        let mut options = Vec::with_capacity(4);
        if let Some(option) = self.next_build_ore(blueprint) {
            options.push(option);
        }
        if let Some(option) = self.next_build_clay(blueprint) {
            options.push(option);
        }
        if let Some(option) = self.next_build_obsidian(blueprint) {
            options.push(option);
        }
        if let Some(option) = self.next_build_geode(blueprint) {
            options.push(option);
        }
        options
    }
}

impl Default for State {
    fn default() -> Self {
        Self {
            time_left: 32,
            ore_bot: 1,
            ore: 0,
            clay_bot: 0,
            clay: 0,
            obsidian_bot: 0,
            obsidian: 0,
            geode: 0,
        }
    }
}

fn compute(blueprint: &Blueprint) -> u32 {
    let mut states = BTreeMap::<u32, HashSet<State>>::new();
    states.entry(32).or_default().insert(State::default());

    let mut best_score = 0;
    for time_left in (1..=32).rev() {
        let time_states = states.remove(&time_left);
        for new_state in time_states
            .iter()
            .flatten()
            .flat_map(|state| state.next_state(blueprint))
        {
            best_score = u32::max(new_state.geode, best_score);
            states
                .entry(new_state.time_left)
                .or_default()
                .insert(new_state);
        }
    }

    best_score
}

fn main() {
    let stdin = stdin();
    let mut scores = Vec::new();

    for _ in 0..3 {
        let mut line = String::new();

        if let Ok(0) = stdin.read_line(&mut line) {
            break;
        }

        let blueprint = line.parse().unwrap();
        let score = compute(&blueprint);
        scores.push(score);
    }
    println!("{}", scores.iter().product::<u32>())
}
