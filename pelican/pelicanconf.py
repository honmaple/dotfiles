#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = 'honmaple'
SITENAME = "HonMaple's Blog"
SITEURL = 'http://honmaple.gq'
# Uncomment following line if you want document-relative URLs when developing
RELATIVE_URLS = True

PATH = 'content'
PAGE_PATHS = ['pages']
ARTICLE_PATHS = ['articles']

TIMEZONE = 'Europe/Paris'

DEFAULT_LANG = 'en'
DEFAULT_PAGINATION = 9
GITHUB_URL = 'http://github.com/honmaple/honmaple.github.io'
DISQUS_SITENAME = 'honmaple'
# Feed generation is usually not desired when developing
#  FEED_ALL_ATOM = None
#  CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None
AUTHOR_FEED_ATOM = None
AUTHOR_FEED_RSS = None

FEED_ALL_ATOM = 'feeds/all.rss.xml'
CATEGORY_FEED_ATOM = 'feeds/%s.rss.xml'

ARCHIVES_SAVE_AS = 'archives/index.html'
ARCHIVES_URL = 'archives/'
ARTICLE_SAVE_AS = 'articles/{date:%Y}/{date:%m}/{slug}.html'
ARTICLE_URL = 'articles/{date:%Y}/{date:%m}/{slug}.html'
PAGE_SAVE_AS = 'pages/{slug}/index.html'
PAGE_URL = 'pages/{slug}'
TAG_SAVE_AS = 'tag/{slug}/index.html'
TAG_URL = 'tag/{slug}/'
TAGS_SAVE_AS = 'tags/index.html'
TAGS_URL = 'tags/'

THEME = 'pelican-themes/subtle'
PYGMENTS_STYLE = 'monokai'
# Blogroll
LINKS = (('Pelican', 'http://getpelican.com/'),
         ('Python.org', 'http://python.org/'),
         ('Jinja2', 'http://jinja.pocoo.org/'),
         ('个人博客', 'https://honmaple.com'),)

# Social widget
SOCIAL = (('GitHub', 'https://github.com/honmaple'),)


PLUGIN_PATHS = ['pelican-plugins']
PLUGINS = [ 'sitemap']

# Sitemap
SITEMAP = {
    'format': 'xml',
    'priorities': {
        'articles': 0.5,
        'indexes': 0.5,
        'pages': 0.5
    },
    'changefreqs': {
        'articles': 'monthly',
        'indexes': 'daily',
        'pages': 'monthly'
    }
}
