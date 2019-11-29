# -*- coding: utf-8 -*-
import re, sys, argparse

class Verse():
    def __init__(self, text, metric):
        self._text = text
        self._metric = metric
    
    def get_text(self):
        return self._text

    def get_metric(self):
        return self._metric

    def get_lily_text(self):
        return "\livretVerse#{} {{ {} }}".format(
            self.get_metric(),
            self.get_text())

class LilyVerse(Verse):
    def __init__(self, text):
        Verse.__init__(self, text, 12)

class LilyShortVerse(Verse):
    def __init__(self, text):
        Verse.__init__(self, text, 10)

class VersePart(LilyVerse):
    def __init__(self, text):
        LilyVerse.__init__(self, text)
        self._prev_part = []
        self._last_part = None

    def set_last_part(self, last_part):
        self._last_part = last_part

    def set_previous_parts(self, parts):
        self._prev_parts = parts

    def get_previous_parts(self):
        return self._prev_parts

    def get_text(self):
        if self._prev_parts == []:
            return Verse.get_text(self)
        else:
            return "\\transparent {{ {} }} {}".format(
                " ".join([Verse.get_text(part) for part in self._prev_parts]),
                Verse.get_text(self))

class VerseLastPart(VersePart):
    def __init__(self, text):
        VersePart.__init__(self, text)

class LilyLine():
    def __init__(self, text):
        self._text = text

    def get_lily_text(self):
        return self._text

class Lilybretto():
    def __init__(self):
        self._lines = []

    def add_line(self, line):
        self._lines.append(line)

    def get_lines(self):
        return self._lines

class RawLibrettoReader():
    def read(self, file):
        libretto = Lilybretto()
        verse_parts = []
        for line in file:
            verse_match = re.match(r"^%#(\S*) (.*)$", line)
            if verse_match:
                # a verse
                cmd = verse_match.group(1)
                verse = verse_match.group(2).strip()
                if cmd == "":
                    # a long verse
                    libretto.add_line(LilyVerse(verse))
                elif cmd == "~":
                    # a short verse
                    libretto.add_line(LilyShortVerse(verse))
                elif cmd == "-":
                    # a split verse
                    verse_part = VersePart(verse)
                    verse_part.set_previous_parts(list(verse_parts))
                    verse_parts.append(verse_part)
                    libretto.add_line(verse_part)
                elif cmd == "=":
                    last_part = VerseLastPart(verse)
                    last_part.set_previous_parts(verse_parts)
                    libretto.add_line(last_part)
                    for part in verse_parts:
                        part.set_last_part(last_part)
                    verse_parts = []
                else:
                    # cmd is expected to be a number
                    print(cmd, " ", verse)
                    libretto.add_line(Verse(verse, int(cmd)))
            else:
                # a LilyPond line
                libretto.add_line(LilyLine(line.rstrip()))
        return libretto

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='LilPond libretto generation.',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        'files', metavar='FILE',
        type=argparse.FileType('r'),
        nargs='+',
        help='input files')
    args = vars(parser.parse_args())
    for file in args['files']:
        reader = RawLibrettoReader()
        libretto = reader.read(file)
        for line in libretto.get_lines():
            print(line.get_lily_text())
