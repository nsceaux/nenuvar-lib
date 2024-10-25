from syllabify import *
import math

def make_centered(text):
    padding = max(0, math.floor((72 - len(text)) / 2))
    return """{}{}""".format(' ' * padding, text)

class LilyVerse(Verse):
    def __init__(self, text):
        Verse.__init__(self, text)

    def get_txt_text(self):
        return """    {}{}""".format(
            ' ' * max(0, 12 - self.get_metric()),
            self.get_text())

class EludedVerse(LilyVerse):
    def __init__(self, text, forced_metric):
        LilyVerse.__init__(self, text)
        self._forced_metric = forced_metric

    def get_metric(self):
        return self._forced_metric

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

    def get_metric(self):
        return self._last_part.get_metric()

    def get_text(self):
        if self._prev_parts == []:
            return Verse.get_text(self)
        else:
            return """{}{}""".format(
                " " * len(' '.join([Verse.get_text(part) for part in self._prev_parts])),
                Verse.get_text(self))


class VerseLastPart(VersePart):
    def __init__(self, text):
        VersePart.__init__(self, text)

    def get_metric(self):
        metric = Verse.get_metric(self)
        for part in self.get_previous_parts():
            metric += Verse.get_metric(part)
        return metric

class LilyLine():
    def __init__(self, text):
        self._text = text

    def syllabify(self, sign_tokenizer = None, syllable_tokenizer = None ):
        pass
    
    def get_txt_text(self):
        text = self._text.replace("\\smallCaps", "")\
        .replace("\\wordwrap-center", "")\
        .replace("\\wordwrap", "")\
        .replace("\\line", "")\
        .replace("\\column", "")\
        .replace("\\justify", "")\
        .replace("\\smaller", "")\
        .replace("\\italic", "")
        text = re.sub(r'%.*', '', text)
        text = re.sub(r'\\hspace#\d+', '', text).strip()
        text = re.sub(r'\\raise#[\d.]+', '', text).strip()
        text = re.sub(r'\\left-brace#\d+', '', text).strip()
        text = re.sub(r'\\right-brace#\d+', '', text).strip()
        text = re.sub(r'\\transparent\s*{([^}]*)}', '', text)
        match_with_brace = re.match(r"^\\(\S*)\s*{(.*)$", text)
        text = text.replace("{", "").strip()
        match = re.match(r"^\\(\S*)(.*)$", text)
        if match:
            cmd = match.group(1).strip()
            rest = match.group(2).replace("}", "").strip()
            ended = not not re.match(r".*}\s*$", match.group(2)) or not match_with_brace
            ending = ""
            if ended: ending = ""
            if cmd == "livretAct":
                return make_centered(rest.upper())
            if cmd == "livretFinAct":
                return make_centered(rest.upper())
            elif cmd == "livretScene":
                return make_centered(rest.upper())
            elif re.match("livretTocRef", cmd):
                return rest.replace('"', '')
            elif re.match("livretRef", cmd):
                return None
            elif cmd == "sep":
                return """                          --------------------"""
            elif cmd == "livretPiece":
                return rest
            elif cmd in ["livretPers", "livretPersVerse", "livretPersNormal"]:
                if rest == "":
                    return None
                else:
                    return f"""{rest} :"""
            elif cmd == "livretPersDidas":
                # \livretPersDidas Character didascalies+
                pers_match = re.match(r'^\s*([\S]+)\s(.*)$', rest)
                character = pers_match.group(1).strip()
                didas = pers_match.group(2).strip()
                return """{} {} :""".format (character, didas)
            elif re.match(r"livretDescAtt.*", cmd):
                if (rest != '' and ending != ''):
                    return """{}{}""".format(rest, ending)
                else:
                    return None
            elif re.match(r"livretText.*", cmd):
                return """{}{}""".format(rest, ending)
            elif re.match(r"livretTitreRef", cmd):
                return """{}{}""".format(re.sub(r'"', '', re.sub(r"#'.*", "", rest)), ending)
            elif re.match(r"livretTitre.*", cmd):
                return """{}{}""".format(rest, ending)
            elif cmd == "null":
                return ""
            elif cmd == "livretDidasPPage" or  cmd == "livretDidasP" or cmd == "livretDidas" or cmd == "livretDidasPage":
                return """{}{}""".format(
                    rest, ending)
            elif cmd == "livretAlt":
                return rest
            elif cmd == "livretAltB":
                return rest
            elif cmd == "livretAltEnd":
                return None
            elif re.match(r"livretVerse", cmd):
                verse_match = re.match(r'livretVerse#(\d+)', cmd)
                metric = verse_match.group(1)
                if rest.strip() == "":
                    return ' ' * max(0, 12 - metric)
                else:
                    # special case: parallel verses
                    if re.search(r'\\column', self._text):
                        return self._get_parallel_verse_html_text(self._text)
                    else:
                        return """{}{}""".format(
                            ' ' * max(0, 12 - metric), rest)
            else:
                return text
        elif re.match(r"^}", self._text):
            return None
        else:
            return text.replace("}", "")

    def _get_parallel_verse_html_text(self, lily_line):
        # \livretVerse#x { xxxxx \column { xx1 xx2 } xxx }
        # or:
        # \livretVerse#x { xxxxx \column { \line { xx1 } \line {  xx2 } } xxx }
        match = re.match(r'\\livretVerse#(\d+)\s*{\s*(.*)\s*}\s*$', lily_line)
        metric = int(match.group(1))
        verse = match.group(2)
        verse = re.sub(r'\\raise#[\d.]+', '', verse).strip()
        verse = re.sub(r'\\left-brace#\d+', '', verse).strip()
        verse = re.sub(r'\\right-brace#\d+', '', verse).strip()
        verse_match = re.match(r'^([^\\]*)\\column\s*{(.*)}([^}]*)$', verse)
        beginning = verse_match.group(1).strip()
        alternatives = verse_match.group(2).strip()
        ending = verse_match.group(3).strip()
        if re.search(r'\\line', alternatives):
            alt_match = re.match(r'^\s*\\line\s*{(.*)}\s*\\line\s*{(.*)}\s*$', alternatives)
        else:
            alt_match = re.match(r'^\s*([^ ]*)\s+([^ ]*)\s*$', alternatives)
        alt1 = alt_match.group(1).strip()
        alt2 = alt_match.group(2).strip()
        html = """{}{} /{}\ {}
{} \{}/""".format(
            ' ' * max(0, 12 - metric), beginning, alt1, ending,
            ' ' * (max(0, 12 - metric) + len(beginning)), alt2)
        return html
                            
class Lilybretto():
    def __init__(self, language):
        self._lines = []
        self.language = language

    def add_line(self, line):
        self._lines.append(line)

    def get_lines(self):
        return self._lines

    def syllabify(self):
        sign_tokenizer = SignTokenizer(language = self.language)
        if self.language == 'fr':
            syllable_tokenizer = SyllableTokenizerWithWordSeparation()
        elif self.language == 'it':
            syllable_tokenizer = SyllableTokenizerIt()
        else:
            syllable_tokenizer = SyllableTokenizer()
        for line in self._lines:
            line.syllabify(sign_tokenizer, syllable_tokenizer)


class RawLibrettoReader():
    def __init__(self, language="fr"):
        self.language = language

    def read(self, file):
        #file = open(filename, 'r')
        libretto = Lilybretto(self.language)
        verse_parts = []
        for line in file:
            verse_match = re.match(r"^%#(\S*) (.*)$", line)
            if verse_match:
                # a verse
                cmd = verse_match.group(1)
                verse = verse_match.group(2).strip()
                if cmd == "" or cmd == "~":
                    # a regular full verse
                    libretto.add_line(LilyVerse(verse))
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
                    # TODO: robustness/error handling
                    # an eluded verse
                    libretto.add_line(EludedVerse(verse, int(cmd)))
            else:
                # a LilyPond line
                libretto.add_line(LilyLine(line.rstrip()))
        return libretto

def print_header(file = sys.stdout, title = 'LIVRET', subtitle = ''):
    
    print("""@@TITLE@@
@@SUBTITLE@@
"""\
          .replace('@@TITLE@@', make_centered(title.upper()))
          .replace('@@SUBTITLE@@', make_centered(subtitle.upper())), file=file)

def print_footer(file=sys.stdout):
    print("", file=file)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='TXT libretto generation.',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '--language',
        default='fr',
        help='verse language (fr, it)')
    parser.add_argument(
        '--title',
        default='LIVRET',
        help='Title')
    parser.add_argument(
        '--subtitle',
        default='',
        help='Subtitle')
    parser.add_argument(
        'files', metavar='FILE',
        type=argparse.FileType('r'),
        nargs='+',
        help='input files')
    args = vars(parser.parse_args())
    print_header(title=args['title'], subtitle=args['subtitle'])
    for file in args['files']:
        reader = RawLibrettoReader(args['language'])
        libretto = reader.read(file)
        libretto.syllabify()
        for line in libretto.get_lines():
            txt = line.get_txt_text()
            if (txt != None):
                print(txt)
    print_footer()
