#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import libcalamares


def run():
    """ Create locale """
    locale_conf = libcalamares.globalstorage.value("localeConf")
    print('localectl run')
    print(locale_conf)

    if not locale_conf:
        locale_conf = {
            'LANG': 'en_US.UTF-8',
            'LC_NUMERIC': 'en_US.UTF-8',
            'LC_TIME': 'en_US.UTF-8',
            'LC_MONETARY': 'en_US.UTF-8',
            'LC_PAPER': 'en_US.UTF-8',
            'LC_NAME': 'en_US.UTF-8',
            'LC_ADDRESS': 'en_US.UTF-8',
            'LC_TELEPHONE': 'en_US.UTF-8',
            'LC_MEASUREMENT': 'en_US.UTF-8',
            'LC_IDENTIFICATION': 'en_US.UTF-8'
        }

    with open("/tmp/locale.nix", "w") as edl:
        edl.write("{\n")
        for k, v in locale_conf.items():
            edl.write("  {!s} = \"{!s}\";\n".format(k, v))
        edl.write("}\n")

    return None
