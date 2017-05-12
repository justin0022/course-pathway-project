import express from 'express'

const app = express()

app.use(express.static('dist'))

app.listen(4000, () => {
     console.log('Listening on port 4000!')
})