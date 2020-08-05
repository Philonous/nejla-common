<template>
  <div class="mt-5">
    <section class="container fade-in" style="max-width: 768px;">
      <h1 class="d-none">Åtkomst till Project</h1>
      <ValidationObserver v-slot="{ handleSubmit }">
        <form @submit.prevent="handleSubmit(onSubmit)" novalidate>
          <div class="mb-5 text-center">
            <img
              alt
              sizes="283px"
              :src="logo283"
              :srcset="`${logo566} 566w, ${logo1132} 1132w, ${logo2264} 2264w`"
              style="height: 48px;"
            />
          </div>
          <div class="mb-5 text-center">
            <b-form-group>
              <div slot="label">
                Om du
                <b v-if="isCreatingAccount">redan har ett Project-konto</b
                ><b v-if="!isCreatingAccount">vill skapa ett Project-konto</b>,
                välj då <b v-if="isCreatingAccount">Logga in på ett konto</b
                ><b v-if="!isCreatingAccount">Skapa ett nytt konto</b> nedan.
              </div>
              <b-form-radio-group
                button-variant="outline-primary"
                buttons
                class="mt-3 w-100"
                :options="[
                  { text: 'Skapa ett nytt konto', value: true },
                  { text: 'Logga in på ett konto', value: false }
                ]"
                v-model="isCreatingAccount"
              />
            </b-form-group>
          </div>
          <!-- Course code -->
          <div class="mb-5" v-if="isCreatingAccount">
            <ValidationProvider
              rules="required|courseCode"
              v-slot="providerContext"
            >
              <b-form-group label="Kurskod">
                <b-form-input
                  ref="courseCode"
                  :state="getState(providerContext)"
                  trim
                  v-model="courseCode"
                />
                <div slot="description">
                  Kurskoden är <u>inte</u> lösenordet för ditt konto. Kurskoden
                  tillhandahålls av din utbildare.
                </div>
                <b-form-invalid-feedback>{{
                  providerContext.errors[0]
                }}</b-form-invalid-feedback>
              </b-form-group>
            </ValidationProvider>
          </div>
          <!-- Name -->
          <div class="mb-5" v-if="isCreatingAccount">
            <ValidationProvider rules="required|names" v-slot="providerContext">
              <b-form-group label="För- och efternamn">
                <b-form-input
                  :state="getState(providerContext)"
                  trim
                  v-model="names"
                />
                <div slot="description">
                  Ditt namn används för identifikation och betygsrapportering.
                </div>
                <b-form-invalid-feedback>{{
                  providerContext.errors[0]
                }}</b-form-invalid-feedback>
              </b-form-group>
            </ValidationProvider>
          </div>
          <!-- E-mail -->
          <div class="mb-5">
            <ValidationProvider rules="required|email" v-slot="providerContext">
              <b-form-group label="E-postadress">
                <b-form-input
                  :state="getState(providerContext)"
                  trim
                  type="email"
                  v-model="email"
                />
                <div slot="description">
                  Din e-postadress används för kursrelaterad återkoppling.
                </div>
                <b-form-invalid-feedback>{{
                  providerContext.errors[0]
                }}</b-form-invalid-feedback>
              </b-form-group>
            </ValidationProvider>
          </div>
          <!-- Password -->
          <div class="mb-5">
            <ValidationProvider rules="required|min:6" v-slot="providerContext">
              <b-form-group
                :label="isCreatingAccount ? 'Önskat lösenord' : 'Lösenord'"
              >
                <b-form-input
                  :state="getState(providerContext)"
                  trim
                  type="password"
                  v-model="password"
                />
                <div slot="description">
                  Välj det lösenord som du vill ha på Project.
                </div>
                <b-form-invalid-feedback>{{
                  providerContext.errors[0]
                }}</b-form-invalid-feedback>
              </b-form-group>
            </ValidationProvider>
          </div>
          <!-- Confirmation password -->
          <div class="mb-5" v-if="isCreatingAccount">
            <ValidationProvider
              :rules="{ required: true, confirmationPassword: password }"
              v-slot="providerContext"
            >
              <b-form-group label="Önskat lösenord, igen">
                <b-form-input
                  :state="getState(providerContext)"
                  trim
                  type="password"
                  v-model="confirmationPassword"
                />
                <div slot="description">
                  Fyll i samma lösenord en gång till.
                </div>
                <b-form-invalid-feedback>{{
                  providerContext.errors[0]
                }}</b-form-invalid-feedback>
              </b-form-group>
            </ValidationProvider>
          </div>
          <!-- Consent -->
          <div class="mb-5" v-if="isCreatingAccount">
            <section class="mb-5">
              <h1 class="h3">Samtycke</h1>
              <p>
                ...
              </p>
            </section>
            <ValidationProvider
              :rules="{ required: { allowFalse: false } }"
              v-slot="providerContext"
            >
              <b-form-group>
                <!-- invalid-feedback="" -->
                <b-form-checkbox
                  size="lg"
                  :state="getState(providerContext)"
                  rules="required"
                  v-model="consent"
                >
                  Jag godkänner ovanstående villkor
                </b-form-checkbox>
                <b-form-invalid-feedback :state="getState(providerContext)"
                  >Samtycke till ovanstående hantering måste lämnas för att
                  kontoregistreringen ska kunna
                  genomföras.</b-form-invalid-feedback
                >
              </b-form-group>
            </ValidationProvider>
          </div>
          <b-alert class="mb-5" v-if="errorMessage" show variant="danger">{{
            errorMessage
          }}</b-alert>
          <div>
            <input
              class="btn btn-primary"
              type="submit"
              :value="isCreatingAccount ? createAnAccountAndSignIn : 'Logga in'"
            />
          </div>
        </form>
      </ValidationObserver>
    </section>
  </div>
</template>
<script>
  import { ValidationObserver, ValidationProvider } from 'vee-validate'

  import logo283 from '@/assets/logo-283w.png'
  import logo566 from '@/assets/logo-566w.png'
  import logo1132 from '@/assets/logo-1132w.png'
  import logo2264 from '@/assets/logo-2264w.png'

  export default {
    components: {
      ValidationObserver,
      ValidationProvider
    },
    created() {
      this.redirectIfSignedIn()

      this.createAnAccountAndSignIn = 'Skapa ett konto och logga in'
      this.logo283 = logo283
      this.logo566 = logo566
      this.logo1132 = logo1132
      this.logo2264 = logo2264
      this.signInOnAnAccount = 'Logga in på ett konto'
    },
    data() {
      return {
        confirmationPassword: '',
        consent: false,
        courseCode: '',
        courseCodeValid: false,
        email: '',
        errorMessage: null,
        isCreatingAccount: true,
        names: '',
        password: '',
        serial: 0
      }
    },
    head() {
      return {
        title: this.isCreatingAccount
          ? this.createAnAccountAndSignIn
          : this.signInOnAnAccount
      }
    },
    layout: 'no-navigation',
    methods: {
      getState({ dirty, valid, validated }) {
        return dirty || validated ? valid : null
      },
      async onSubmit() {
        if (this.isCreatingAccount) {
          try {
            await this.$axios.post('/api/create-account', {
              email: this.email,
              name: this.names,
              password: this.password
            })

            try {
              await this.signIn()

              this.$axios.post('/api/courses/signup', {
                code: this.courseCode
              })

              this.errorMessage = null
            } catch (error) {
              this.errorMessage =
                'Ditt konto kunde skapas, men ett kursregistreringsproblem uppstod. Kontakta din utbildare.'
            }
          } catch (error) {
            if (error.response.status === 409) {
              this.errorMessage =
                'Det finns redan ett Project-konto med den angivna e-postadressen.'
            } else {
              this.errorMessage =
                'Ditt konto kunde inte skapas. Kontakta din utbildare.'
            }
          }
        } else {
          try {
            await this.signIn()

            this.errorMessage = null
          } catch (error) {
            if (error.request.status === 403) {
              this.errorMessage =
                'E-postadressen eller lösenordet som angavs stämmer inte. Om du har förlorat ditt lösenord, kontakta din utbildare.'
            } else {
              this.errorMessage =
                'Ett fel uppstod i samband med inloggningen. Om felet kvarstår, kontakta din utbildare.'
            }
          }
        }
      },
      async redirectIfSignedIn() {
        try {
          await this.$axios('/api/user-info')
          this.$router.push('/')
        } catch (error) {}
      },
      setCreatingAccount(isCreatingAccount) {
        this.isCreatingAccount = isCreatingAccount
      },
      async signIn() {
        this.$store.commit(
          'setToken',
          (
            await this.$axios.post('/api/login', {
              password: this.password,
              user: this.email
            })
          ).data.token.token
        )

        await this.$store.dispatch('fetchCourses')

        this.$router.push('/')
      }
    }
  }
</script>
