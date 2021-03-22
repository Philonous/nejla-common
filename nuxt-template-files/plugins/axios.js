export default function({ $axios, redirect, store }) {
  $axios.onResponseError((error) => {
    if (error.response && error.reponse.status === 403) {
      if (process.server) {
        redirect('/sign-in')
      } else {
        store.commit('setToken', null)
      }
    }
  })
}
