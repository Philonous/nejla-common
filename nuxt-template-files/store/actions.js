import cookie from 'cookie'

export default {
  async nuxtServerInit({ commit, dispatch }, { req }) {
    if (req.headers.cookie) {
      const token = cookie.parse(req.headers.cookie).token

      if (token) {
        commit('setToken', token)
      }
    }

    try {
      await dispatch('fetchCourses')
    } catch (error) {}
  }
}
