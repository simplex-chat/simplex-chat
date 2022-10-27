module.exports = {
  darkMode : 'class',
  content: ["./src/**/*.{html,js,njk}"],
  theme: {
    extend: {
      backgroundImage: {
        'gradient-radial': 'radial-gradient(88.77% 102.03% at 92.64% -13.22%, #17203D 0%, #0C0B13 100%)',
        'gradient-radial-mobile': 'radial-gradient(77.4% 73.09% at -3.68% 100%, #17203D 0%, #0C0B13 100%)',
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
        'secondary-bg-light': '#F3F6F7',
        'primary-bg-light': '#FFFFFF',
        'secondary-bg-dark': '#11182F',
        'primary-bg-dark': '#0C0B13',


        // What makes SimpleX private
        'card-bg-light': '#ffffff',
        'card-desc-bg-light': '#D9E7ED', 
        'card-bg-dark': '#17203D',
        'card-desc-bg-dark': '#1B325C',
      }
    },
  },
  plugins: [],
}
