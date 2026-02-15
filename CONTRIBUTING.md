# Contributing to HWM

## Development Setup

```bash
git clone https://github.com/nalchevanidze/hwm
cd hwm
stack build
stack test
```

## Making Changes

1. Fork the repo
2. Create a feature branch: `git checkout -b feature-name`
3. Make your changes
4. Run tests: `stack test`
5. Push and create a PR

## Code Style

- Follow existing code patterns
- Run `ormolu` for formatting
- Keep functions focused and documented

## Releases

This project uses [relasy](https://github.com/nalchevanidze/relasy) for automated releases.

## Reporting Issues

Use GitHub Issues with:
- Clear description
- Steps to reproduce
- Expected vs actual behavior
- HWM version (`hwm version`)

## Questions?

Open a discussion or issue.
