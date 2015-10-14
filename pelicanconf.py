# -*- coding: utf-8 -*-
from __future__ import unicode_literals

AUTHOR = 'honmaple'
AUTHOR_EMAIL = 'xiyang0807@gmail.com'
SITENAME = "HonMaple"
SITEURL = 'http://honmaple.github.io'
TIMEZONE = "Asia/Shanghai"
DEFAULT_DATE_FORMAT = ('%Y-%m-%d')

DEFAULT_LANG = 'zh'
# can be useful in development, but set to False when you're ready to publish
RELATIVE_URLS = True

GITHUB_USER = 'honmaple'
GITHUB_SKIP_FORK = True
GITHUB_SHOW_USER_LINK = True
# GITHUB_URL = 'http://github.com/honmaple/honmaple.github.io'
# DISQUS_SITENAME = "HonMaple"
REVERSE_CATEGORY_ORDER = True
DEFAULT_PAGINATION = 4

DEFAULT_METADATA = {}
PATH = 'content'
PAGE_PATHS = ['pages']
ARTICLE_PATHS = ['articles']
CUSTOM_CSS = 'static/custom.css'
EXTRA_PATH_METADATA = {
    'extra/robots.txt': {'path': 'robots.txt'},
	'extra/custom.css': {'path': 'static/custom.css'},
    }
STATIC_PATHS = [
    'images',
    'extra/robots.txt',
	'extra/custom.css',
    ]

ARTICLE_URL = 'articles/{date:%Y}/{date:%b}/{slug}.html'
ARTICLE_SAVE_AS = 'articles/{date:%Y}/{date:%b}/{slug}.html'
PAGE_URL = '{slug}.html'
PAGE_SAVE_AS = '{slug}.html'
# CATEGORY_URL = '{slug}/index.html'
# CATEGORY_SAVE_AS = CATEGORY_URL
TAG_URL = 'tags/{slug}.html'
TAG_SAVE_AS = TAG_URL
TAGS_SAVE_AS = 'tags.html'

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
THEME = 'pelican-themes/pelican-bootstrap3'
# SEARCH_URL = 'tag/search.html'
# SEARCH_SAVE_AS = SEARCH_URL
DISPLAY_PAGES_ON_MENU = True
DISPLAY_CATEGORIES_ON_MENU = True
DISPLAY_TAGS_ON_SIDEBAR = True
DISPLAY_ARTICLE_INFO_ON_INDEX = True
AVATAR = "images/head.png"
BOOTSTRAP_THEME = 'yeti'
PYGMENTS_STYLE = 'monokai'
DIRECT_TEMPLATES = ('index', 'categories', 'authors', 'archives',  'search')
SHOW_ABOUTME = True
ABOUT_ME = """
I like solitude, yearning for freedom
<br/>
"""

# Plugin
PLUGIN_PATHS = ['pelican-plugins']
PLUGINS = [ 'sitemap', 'gravatar', 'tipue_search']

CC_LICENSE = "CC-BY"

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

