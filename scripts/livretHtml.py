from syllabify import *

class LilyVerse(Verse):
    def __init__(self, text):
        Verse.__init__(self, text)

    def get_html_text(self):
        return """<div class="ver{}">{}</div>""".format(
            self.get_metric(),
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
            return """<span class="transparent">{}</span>{}""".format(
                " ".join([Verse.get_text(part) for part in self._prev_parts]),
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

    def get_html_text(self):
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
        text = re.sub(r'\\transparent\s*{([^}]*)}', '<span class="transparent">\\1</span>', text)
        match_with_brace = re.match(r"^\\(\S*)\s*{(.*)$", text)
        text = text.replace("{", "").strip()
        match = re.match(r"^\\(\S*)(.*)$", text)
        if match:
            cmd = match.group(1).strip()
            rest = match.group(2).replace("}", "").strip()
            ended = not not re.match(r".*}\s*$", match.group(2)) or not match_with_brace
            ending = ""
            if ended: ending = "</div>"
            if cmd == "livretAct":
                return "<h2>{}</h2>".format(rest)
            if cmd == "livretFinAct":
                return """<div class="fin">{}</div>""".format(rest)
            elif cmd == "livretScene":
                return "<h3>{}</h3>".format(rest)
            elif re.match("livretTocRef", cmd):
                return f"""<h4>{rest.replace('"', '')}</h4>"""
            elif re.match("livretRef", cmd):
                return ""
            elif cmd == "sep":
                return """<div class="sep">&nbsp;</div>"""
            elif cmd == "livretPiece":
                return """<div class="piece">{}</div>""".format(rest)
            elif cmd in ["livretPers", "livretPersVerse", "livretPersNormal"]:
                if cmd == "livretPersNormal":
                    style = "didas"
                else:
                    style = "perso"
                if rest == "":
                    return f"""<div class="{style}">"""
                else:
                    extra_ending = ""
                    if re.match(r'.*{\s*$', self._text):
                        extra_ending = "<div>"
                    return f"""<div class="{style}">{rest}</div>{extra_ending}"""
            elif cmd == "livretPersDidas":
                # \livretPersDidas Character didascalies+
                rest = rest.replace(' ', '&nbsp;')
                pers_match = re.match(r'^\s*([\S]+)\s(.*)$', rest)
                character = pers_match.group(1).strip()
                didas = pers_match.group(2).strip()
                return """<div class="perso">{} <span class="didas">{}</span></div>""".format(
                    character, didas)
            elif re.match(r"livretDescAtt.*", cmd):
                return """<div class="desc">{}{}""".format(rest, ending)
            elif re.match(r"livretText.*", cmd):
                return """<div class="text">{}{}""".format(rest, ending)
            elif re.match(r"livretTitre.*", cmd):
                return """<div class="titre">{}{}""".format(rest, ending)
            elif cmd == "null":
                return "<div>&nbsp;</div>"
            elif cmd == "livretDidasPPage" or  cmd == "livretDidasP" or cmd == "livretDidas" or cmd == "livretDidasPage":
                return """<div class="didas">{}{}""".format(
                    rest, ending)
            elif cmd == "livretAlt":
                return """<div class="alternative"><div class="alternativeTitle">{}</div>""".format(rest)
            elif cmd == "livretAltB":
                return """</div><div class="alternative"><div class="alternativeTitle">{}</div>""".format(rest)
            elif cmd == "livretAltEnd":
                return "</div>"
            elif re.match(r"livretVerse", cmd):
                verse_match = re.match(r'livretVerse#(\d+)', cmd)
                metric = verse_match.group(1)
                if rest.strip() == "":
                    return """<div class="ver{}">""".format(metric)
                else:
                    # special case: parallel verses
                    if re.search(r'\\column', self._text):
                        return self._get_parallel_verse_html_text(self._text)
                    else:
                        return """<div class="ver{}">{}</div>""".format(
                            metric, rest)
            else:
                return text
        elif re.match(r"^}", self._text):
            return "</div>"
        else:
            return text.replace("}", "")

    def _get_parallel_verse_html_text(self, lily_line):
        # \livretVerse#x { xxxxx \column { xx1 xx2 } xxx }
        # or:
        # \livretVerse#x { xxxxx \column { \line { xx1 } \line {  xx2 } } xxx }
        match = re.match(r'\\livretVerse#(\d+)\s*{\s*(.*)\s*}\s*$', lily_line)
        metric = match.group(1)
        verse = match.group(2)
        # on retire les trucs dont on ne se sert pas ici
        verse = re.sub(r'\\raise#[\d.]+', '', verse).strip()
        verse = re.sub(r'\\left-brace#\d+', '', verse).strip()
        verse = re.sub(r'\\right-brace#\d+', '', verse).strip()
        # <début> \column { <alternatives...> } <fin>
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
        html = """<div class="ver{}">{} <div class="altverse"><div>{}</div><div>{}</div></div> {}</div>""".format(
            metric, beginning, alt1, alt2, ending)
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
    
    print("""
<html>
  <head>
    <meta charset="UTF-8">
    <title>@@Title@@ @@Subtitle@@ — Livret</title>
    <link href="http://fonts.googleapis.com/css?family=Garamond" rel="stylesheet" type="text/css">
    <style>
      body {
        justify-content: center;
        display: flex;
      }
      .livret {
         width: 30em;
         padding: 5 5 5 5;
         margin: 10 10 10 10;
         font-family: 'Garamond', serif;
      }
      h1 { text-align: center; }
      h2 { text-align: center; }
      h3 { text-align: center; }
      .desc {
        text-align: center;
        font-style: italic;
      }
      .sep {
        margin: 0 auto;
        width: 12em;
        border-bottom: solid 1px black;
      }
      .didas {
        font-style: italic;
        font-align: justify;
        font-size: 80%;
        font-variant: normal;
      }
      .text {
        font-align: justify;
        font-variant: normal;
      }
      .titre {
        font-align: justify;
        font-weight: bold;
        font-variant: normal;
        margin-top: 10px;
      }
      .fin {
        margin-top: 1ex;
        font-size: 150%;
        font-weight: bold;
        text-align: center;
      }
      .perso {
        font-variant: small-caps;
      }
      .ver13 { padding-left: 2em; }
      .ver12 { padding-left: 2em; }
      .ver11 { padding-left: 2em; }
      .ver10 { padding-left: 4em; }
      .ver9 { padding-left: 5em; }
      .ver8 { padding-left: 6em; }
      .ver7 { padding-left: 7em; }
      .ver6 { padding-left: 8em; }
      .ver5 { padding-left: 9em; }
      .ver4 { padding-left: 10em; }
      .ver3 { padding-left: 11em; }
      .ver2 { padding-left: 12em; }
      .ver1 { padding-left: 13em; }
      .ver0 { padding-left: 14em; }
      .transparent { opacity: 0; }
      .altverse {
        display: inline-block;
        vertical-align: middle;
        border-left: 1px dotted black;
        border-right: 1px dotted black;
        padding-left: 2px;
        padding-right: 2px;
      }
      .alternative {
        border: 1px dotted black;
        margin-bottom: 3px;
      }
      .alternativeTitle {
        font-size: 80%;
        text-align: right;
      }
    </style>
  </head>
  <body>
    <div class="livret">
      <h1>@@TITLE@@<br>@@SUBTITLE@@</h1>
"""\
          .replace('@@Title@@', title)\
          .replace('@@TITLE@@', title.upper())
          .replace('@@Subtitle@@', subtitle)\
          .replace('@@SUBTITLE@@', subtitle.upper()), file=file)

def print_footer(file=sys.stdout):
    print("""
      </div>
    </div>
  </body>
</html>
""", file=file)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='HTML libretto generation.',
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
            print(line.get_html_text())
    print_footer()
