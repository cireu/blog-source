module.exports = {
    theme: '@vuepress/theme-blog',
    title: 'NIL',
    base: '/',
    markdown: {
        lineNumbers: true,
        toc: {
            includeLevel: [1, 2]
        }
    },
    themeConfig: {
        nav: [
            { text: '首页', link: '/' },
            { text: '标签', link: '/tag/' },
            { text: '关于', link: '/contact' }
        ]
    }
}
