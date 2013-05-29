#Google Search Engine Robot
User-agent: Googlebot
Allow: /?_escaped_fragment_

Allow: /search?q=%23
Disallow: /search/realtime
Disallow: /search/users
Disallow: /search/*/grid

Disallow: /*?
Disallow: /*/followers
Disallow: /*/following

#Yahoo! Search Engine Robot
User-Agent: Slurp
Allow: /?_escaped_fragment_

Allow: /search?q=%23
Disallow: /search/realtime
Disallow: /search/users
Disallow: /search/*/grid

Disallow: /*?
Disallow: /*/followers
Disallow: /*/following

#Yandex Search Engine Robot
User-agent: Yandex
Allow: /?_escaped_fragment_

Allow: /search?q=%23
Disallow: /search/realtime
Disallow: /search/users
Disallow: /search/*/grid

Disallow: /*?
Disallow: /*/followers
Disallow: /*/following

#Microsoft Search Engine Robot
User-Agent: msnbot
Allow: /?_escaped_fragment_

Allow: /search?q=%23
Disallow: /search/realtime
Disallow: /search/users
Disallow: /search/*/grid

Disallow: /*?
Disallow: /*/followers
Disallow: /*/following

# Every bot that might possibly read and respect this file.
User-agent: *
Allow: /search?q=%23
Disallow: /search/realtime
Disallow: /search/users
Disallow: /search/*/grid

Disallow: /*?
Disallow: /*/followers
Disallow: /*/following
Disallow: /oauth
Disallow: /1/oauth

# Wait 1 second between successive requests. See ONBOARD-2698 for details.
Crawl-delay: 1

# Independent of user agent. Links in the sitemap are full URLs using https:// and need to match
# the protocol of the sitemap.
Sitemap: https://twitter.com/sitemap.xml