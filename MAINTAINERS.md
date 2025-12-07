# Maintainers

This document lists the maintainers of the polyglot-i18n project and their areas of responsibility.

## Project Lead

**hyperpolymath** (@hyperpolymath)
- **Role:** Project Lead, Core Maintainer
- **Responsibilities:**
  - Overall project direction and governance
  - Core library architecture
  - ReScript/WASM components
  - Guix/Nix packaging
  - Container infrastructure
  - Release management and versioning
  - Security vulnerability coordination
- **Time:** 2025-present

## Original Author

This project is a fork of [i18n-node](https://github.com/mashpie/i18n-node), originally created by:

**Marcus Spiegel** (@mashpie)
- **Email:** marcus.spiegel@gmail.com
- **Role:** Original Author (i18n-node)
- **Contributions:**
  - Core translation engine design
  - Original API design (__(), __n(), __mf(), etc.)
  - Express/restify integration patterns
  - MessageFormat and Mustache integration
- **Time:** 2011-2025

We are deeply grateful for Marcus Spiegel's foundational work that made this project possible.

## Core Maintainers

Currently seeking additional core maintainers. See [CONTRIBUTING.md](CONTRIBUTING.md) for how to become a maintainer.

## Component Maintainers

### ReScript Core
**Status:** Seeking maintainers
- **Components:** `bindings/rescript/`, `src/*.res`
- **Responsibilities:**
  - ReScript type definitions and bindings
  - Core translation logic migration
  - Type safety across FFI boundaries
  - Performance optimization

### WASM Runtime
**Status:** Seeking maintainers
- **Components:** `wasm/`, `crates/`
- **Responsibilities:**
  - Rust WASM module maintenance
  - Performance-critical path optimization
  - Memory management and safety
  - Cross-platform compatibility

### Guix/Nix Packaging
**Status:** Seeking maintainers
- **Components:** `guix.scm`, `channels.scm`, `flake.nix`
- **Responsibilities:**
  - GNU Guix package definitions
  - Nix flake maintenance
  - Reproducible build infrastructure
  - Development environment manifests

### Container Infrastructure
**Status:** Seeking maintainers
- **Components:** `container/`, `Containerfile`, `apko.yaml`
- **Responsibilities:**
  - Chainguard Wolfi image maintenance
  - Multi-arch build support
  - Security hardening
  - Container composition

### Deno Runtime
**Status:** Seeking maintainers
- **Components:** `deno/`, `mod.ts`
- **Responsibilities:**
  - Deno module maintenance
  - TypeScript definitions
  - Deno Deploy compatibility
  - Runtime parity with Node.js

### CLI & Documentation
**Status:** Seeking maintainers
- **Components:** `tools/cli.js`, `man/`, `docs/`
- **Responsibilities:**
  - CLI command implementation
  - Man page maintenance
  - AsciiDoc documentation
  - Example maintenance

### Framework Examples
**Status:** Seeking maintainers
- **Components:** `examples/*/`
- **Responsibilities:**
  - Framework integration examples
  - Updating examples for framework changes
  - Documentation accuracy
  - New framework additions

## Emeritus Maintainers

List of former maintainers who have contributed significantly to the project:

- **Marcus Spiegel** (@mashpie) - Original author of i18n-node (2011-2025)

We thank all emeritus maintainers for their contributions and service to the project.

## Becoming a Maintainer

### Criteria

To become a maintainer, you should:

1. **Sustained Contribution:** Have a track record of quality contributions over 3+ months
2. **Domain Expertise:** Demonstrate expertise in your proposed maintenance area
3. **Community Engagement:** Actively participate in discussions, reviews, and support
4. **Alignment:** Understand and align with project goals (ReScript-first, WASM-accelerated, polyglot)
5. **Availability:** Commit to regular maintenance activities (minimum 4-8 hours/month)

### Process

1. **Express Interest:** Open a discussion in GitHub Discussions under "Maintainer Applications"
2. **Nomination:** Existing maintainers may also nominate contributors
3. **Review:** Current maintainers review the application/nomination
4. **Vote:** Existing maintainers vote (requires unanimous approval for <5 maintainers, 75% for 5+)
5. **Onboarding:** New maintainers receive:
   - Repository write access
   - NPM/JSR publish rights (after 3 months)
   - Security advisory access
   - Maintainer documentation
   - Pair programming sessions with existing maintainers

## Maintainer Responsibilities

### Code Review
- Review pull requests in your area of responsibility within 7 days
- Provide constructive feedback aligned with [CONTRIBUTING.md](CONTRIBUTING.md)
- Ensure code quality, test coverage, and documentation standards

### Security
- Respond to security reports within timeframes specified in [SECURITY.md](SECURITY.md)
- Coordinate security patches and releases
- Participate in security audits and threat modeling

### Community
- Monitor and respond to issues and discussions
- Welcome new contributors and provide mentorship
- Enforce Code of Conduct fairly and consistently
- Represent the project professionally

### Releases
- Participate in release planning and versioning decisions
- Review and approve CHANGELOG entries
- Test pre-release builds
- Communicate breaking changes and migration paths

### Documentation
- Maintain documentation in your area
- Review documentation PRs
- Ensure examples and guides remain current

## Maintainer Privileges

- **Write Access:** Direct commit access to the repository
- **Release Authority:** Ability to publish releases to npm/JSR
- **Security Access:** Access to private security advisories
- **Decision Making:** Vote on major project decisions
- **Recognition:** Listed in humans.txt, MAINTAINERS.md, and project documentation
- **Priority Support:** Direct communication channels with other maintainers

## Stepping Down

Maintainers may step down at any time by:

1. Notifying other maintainers via private communication
2. Providing 30-day transition period (if possible)
3. Transferring responsibilities to another maintainer
4. Moving to emeritus status

We respect maintainer time and energy, and stepping down is welcomed without stigma.

## Maintainer Inactivity

If a maintainer is inactive for 6+ months without prior notice:

1. Other maintainers will attempt to contact them
2. After 90 days of no response, maintainer privileges may be revoked
3. Maintainer moves to emeritus status
4. Can be reinstated by re-application

## Decision Making

### Consensus Model

- **Minor Changes:** Any maintainer can merge (e.g., docs, bug fixes, examples)
- **Moderate Changes:** Requires approval from 1 other maintainer (e.g., features, refactors)
- **Major Changes:** Requires approval from all active maintainers or 75% vote (e.g., breaking changes, architecture, licensing)

### Conflict Resolution

1. **Discussion:** Maintainers discuss disagreements in private maintainer channel
2. **Mediation:** Project lead mediates if consensus not reached
3. **Vote:** If mediation fails, vote is called (simple majority)
4. **Documentation:** Decision and rationale documented in GitHub Discussions

## Communication Channels

### Public
- **GitHub Issues:** Bug reports, feature requests
- **GitHub Discussions:** General questions, ideas, RFCs
- **GitHub Pull Requests:** Code contributions

### Private (Maintainers Only)
- **Security Advisories:** GitHub Security Advisory
- **Maintainer Discussion:** Private maintainer channel (to be established)
- **Emergency Contact:** Email to project lead

## Maintainer Meetings

**Status:** Not currently scheduled

When multiple maintainers exist:
- **Frequency:** Monthly (first Thursday, 15:00 UTC)
- **Format:** Video call (Jitsi Meet, open source)
- **Agenda:** Posted 48 hours in advance
- **Notes:** Published in GitHub Discussions (private discussions excluded)

## Governance Evolution

This governance structure is designed for current project scale. As the project grows:

- May adopt more formal governance (e.g., Technical Steering Committee)
- May establish Working Groups for specialized areas (ReScript, WASM, Packaging)
- May create Contributor Ladder (contributor → committer → maintainer → TSC)

Changes to governance require approval from all active maintainers.

## Contact

- **General:** Open a GitHub Discussion
- **Security:** See [SECURITY.md](SECURITY.md)

---

**Last Updated:** 2025-12-07
**Document Version:** 2.0
**Review Cycle:** Quarterly
