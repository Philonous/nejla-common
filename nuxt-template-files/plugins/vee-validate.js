import axios from 'axios'
import { extend } from 'vee-validate'
import { email, is, min, required } from 'vee-validate/dist/rules.umd'

function numberToMaybeWordString(number) {
  switch (number) {
    case 0:
      return 'noll'
    case 1:
      return 'ett'
    case 2:
      return 'två'
    case 3:
      return 'tre'
    case 4:
      return 'fyra'
    case 5:
      return 'fem'
    case 6:
      return 'sex'
    case 7:
      return 'sju'
    case 8:
      return 'åtta'
    case 9:
      return 'nio'
    case 10:
      return 'tio'
    case 11:
      return 'elva'
    case 12:
      return 'tolv'
    default:
      return String(number)
  }
}

extend('courseCode', {
  message: 'Kurskoden är inte giltig.',
  validate: (() => {
    let serialClosure = 0

    return async (value) => {
      serialClosure++

      const serialLocal = serialClosure,
        result = await axios(`/api/public/courses/code/${value}`)

      // Discard out-of-date validity results
      if (serialClosure === serialLocal) {
        return result.data.valid
      }
    }
  })()
})

extend(
  'email',
  Object.assign(email, {
    message: 'Ange en giltig e-postadress.'
  })
)

extend(
  'confirmationPassword',
  Object.assign(is, {
    message: 'De angivna lösenorden stämmer inte överens.'
  })
)

extend(
  'min',
  Object.assign(min, {
    message(_, { length }) {
      return `Ange ett lösenord bestående av minst ${numberToMaybeWordString(
        length
      )} tecken.`
    }
  })
)

extend('names', (value) => {
  if (value.split(/\s+/).length < 2) {
    return 'Ange både förnamn och efternamn.'
  }

  return true
})

extend(
  'required',
  Object.assign(required, {
    message: 'Fältet är obligatoriskt.'
  })
)
