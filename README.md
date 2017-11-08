# nixos-installer
A webserver that can install NixOS for you.

## Running it

The project is not ready for real use yet, but if you'd like to jump in and
help, here's how:

    nix-shell --pure --run ./run

## Contributing

Since this is still mostly in the proof-of-concept stage, I can't promise
that PRs will be promptly merged or stay mergable for long since there's
still heavy refactoring going on, and nothing is set in stone yet.

However there are some things that I'd highly appreciate:

* Additional translations (untranslated strings default to English).
* Artwork to make the install steps look less sterile.
* Additional Overlays for `configuration.nix` to support some use-case you have.
* Ideas and/or hardware for running multiple test scenarios.
* Donations in the form of T-Shirts ;)

I'll try to [keep a log of what's going on](https://github.com/manveru/nixos-installer/projects),
and you're free to contact me via IRC (`#nixos-installer` on freenode)
or on [Keybase](https://keybase.io/manveru).

## Philosophy

1. Use software to detect a problem, and tell users only when there is a
   problem. An error reporting system may be a manual that appears on demand.
2. Make sure that all settings are needed. Eliminate unnecessary settings.
3. Less configuration up front, simplify the installer. Move as much
   configuration to the runtime as possible.
4. Avoid doing things in the installer that also have an UI during run time.
5. Don’t force the user to reenter values that have already been entered.
6. Users should be able to test settings immediately to avoid problems later.
7. All input and output is defined and controlled by the installer. Don’t depend
   on external configuration.
8. Keep the number of modes to a minimum, default new installations to a single
   mode.
9. Require only the bare minimum to run. Provide additional configuration after
   installation.
10. Don’t ask the user to do something that can easily be automated.
11. Make use of provided settings across the whole system to avoid duplication.
12. Provide links to further documentation for external technology.
13. All settings should have a test if possible. A manual test while configuring
    the value, and for critical values a test when the system starts.
14. Make manuals consistent with installation process.
15. Don’t use the manual to fix poorly implemented software. Fix the software
    first.
