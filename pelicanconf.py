# -*- coding: utf-8 -*-
from __future__ import unicode_literals

AUTHOR = 'honmaple'
AUTHOR_EMAIL = 'xiyang0807@gmail.com'
SITENAME = "HonMaple"
TAGLINE = 'I like solitude, yearning for freedom'
SITEURL = 'http://honmaple.github.com'
TIMEZONE = "Asia/Shanghai"
DEFAULT_DATE_FORMAT = ('%Y-%m-%d')

DEFAULT_LANG = 'zh'
# can be useful in development, but set to False when you're ready to publish
RELATIVE_URLS = True

GITHUB_URL = 'http://github.com/honmaple/honmaple.github.io'
DISQUS_SITENAME = "HonMaple"
REVERSE_CATEGORY_ORDER = True
LOCALE = "C"
DEFAULT_PAGINATION = 4

DEFAULT_METADATA = {}
PATH = 'content'
PAGE_PATHS = ['pages']
ARTICLE_PATHS = ['articles']
EXTRA_PATH_METADATA = {
    'extra/robots.txt': {'path': 'robots.txt'},
    }
STATIC_PATHS = [
    'images',
    'extra/robots.txt',
    ]

# URL settings
# PAGINATION_PATTERNS = (
  # (1, '{base_name}/', '{base_name}/index.html'),
  # (2, '{base_name}/page/', '{base_name}/page/{number}.html'),
# )
ARTICLE_URL = 'articles/{slug}.html'
ARTICLE_SAVE_AS = 'articles/{slug}.html'
PAGE_URL = 'pages/{slug}.html'
PAGE_SAVE_AS = 'pages/{slug}.html'

FEED_ALL_ATOM = 'feeds/all.rss.xml'
CATEGORY_FEED_ATOM = 'feeds/%s.rss.xml'

LINKS = (('Pelican', 'http://getpelican.com/'),
         ('Python.org', 'http://python.org/'),
         ('Openshit', 'http://honmaple.tk'),
)

# Social widget
SOCIAL = (
    ('GitHub', 'http://github.com/honmaple'),
)

# Theme
THEME = 'pelican-themes/tuxlite_tbs'
COVER_BG_COLOR = '#375152'

# Plugin
PLUGIN_PATHS = ['pelican-plugins']
PLUGINS = [ 'sitemap', 'gravatar' ]


# Sitemap
SITEMAP = {
  'format': 'xml',
  'priorities': {
    'articles': 1,
    'pages': 0.9,
    'indexes': 0.8,
  },
  'changefreqs': {
    'indexes': 'daily',
    'articles': 'daily',
    'pages': 'weekly'
  }
}

