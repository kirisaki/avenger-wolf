import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Main.embed(document.getElementById('root'));
app.ports.title.subscribe(function(title) {
    document.title = title;
});
registerServiceWorker();
