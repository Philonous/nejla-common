export default {
  axios: {
    browserBaseURL: '/'
  },
  bootstrapVue: {
    bootstrapCSS: false,
    bootstrapVueCSS: false,
    icons: true
  },
  build: {
    postcss: {
      plugins: {
        autoprefixer: {},
        'postcss-import': {}
      }
    }
  },
  css: ['@/assets/main.scss'],
  head: {
    titleTemplate: '%s | Project'
  },
  modules: ['@nuxtjs/axios', '@nuxtjs/proxy', 'bootstrap-vue/nuxt'],
  plugins: ['@/plugins/axios.js', '@/plugins/vee-validate.js'],
  proxy: {
    '/api': {
      pathRewrite: {
        '^/api': '/api'
      },
      target:
        process.env.NODE_ENV === 'production'
          ? 'http://frontend'
          : 'https://example.com'
    }
  }
}
