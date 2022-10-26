module.exports = {
  darkMode : 'class',
  content: ["./src/**/*.{html,js,njk}"],
  theme: {
    extend: {
      backgroundImage: {
        'gradient-radial': 'radial-gradient(88.77% 102.03% at 92.64% -13.22%, #17203D 0%, #0C0B13 100%)',
        'gradient-radial-mobile': 'radial-gradient(77.4% 73.09% at -3.68% 100%, #17203D 0%, #0C0B13 100%)'
      },
      colors: {
        'primary-light': '#0053D0',
        'primary-dark': '#70F0F9',
        'black': '#0D0E12'
      }
    },
  },
  plugins: [],
}
