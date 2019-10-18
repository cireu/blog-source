module.exports = {
    theme: '@vuepress/theme-blog',
    title: 'NIL',
    base: '/',
    markdown: {
        lineNumbers: true,
    },
    plugins: [
        ['container', {
            type: 'tip',
            defaultTitle: '提示',
        }],
        ['container', {
            type: 'danger',
            defaultTitle: '注意',
        }],
        ['container', {
            type: 'warning',
            defaultTitle: '警告',
        }],
        ['rss', {
            base_url: '/',
            site_url: 'https://cireu.github.io',
            count: 20,
            copyright: 'Zhu Zihao'
        }],
    ],
    themeConfig: {
        nav: [
            { text: '首页', link: '/' },
            { text: '标签', link: '/tag/' },
            { text: '关于', link: '/about' },
            { text: '友情链接', link: '/links' },
            { text: 'RSS', link: '/rss.xml' },
        ]
    }
}
