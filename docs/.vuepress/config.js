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
    ],
    themeConfig: {
        nav: [
            { text: '首页', link: '/' },
            { text: '标签', link: '/tag/' },
            { text: '关于', link: '/contact' }
        ]
    }
}
