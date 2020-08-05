FROM node

WORKDIR /app
ADD . /app

RUN npm install
RUN NUXT_TELEMETRY_DISABLED=1 npx nuxt build

ENV HOST 0.0.0.0

EXPOSE 3000

CMD ["npx", "nuxt", "start"]