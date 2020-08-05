export default function ({ redirect, store }) {
  if (!store.state.token) {
    return redirect('/sign-in')
  }
}
