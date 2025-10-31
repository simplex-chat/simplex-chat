module.exports = {
  darkMode : 'class',
  content: ["./src/**/*.{html,js,njk}"],
  theme: {
    extend: {
      backgroundImage: {
        'gradient-radial': 'radial-gradient(88.77% 102.03% at 92.64% -13.22%, #071C46 0%, #000832 100%)',
        'gradient-radial-mobile': 'radial-gradient(77.4% 73.09% at -3.68% 100%, #071C46 0%, #000832 100%)',
      },
      colors: {
        'primary-light': '#0053D0',
        'primary-hover-light': '#1661D1',
        'primary-pressed-light': '#407AD2',
        'primary-dark': '#70F0F9',
        'primary-hover-dark': '#66D9E2',
        'primary-pressed-dark': '#52ABB4',

        'active-blue': '#0197FF',
        'black': '#0D0E12',
        'grey-black': '#3F484B',
        'secondary-bg-light': '#F3FAFF',
        'primary-bg-light': '#FFFFFF',
        'secondary-bg-dark': '#0B2A59',
        'primary-bg-dark': '#000832',


        // What makes SimpleX private
        'card-bg-light': '#ffffff',
        'card-desc-bg-light': '#DBEEFF',
        'card-bg-dark': '#071C46',
        'card-desc-bg-dark': '#1B325C',
      }
    },
  },
  plugins: [],
}
