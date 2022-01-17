import { makeStyles } from "@material-ui/styles";

export default makeStyles(theme => ({
    root: {
        maxWidth: "100vw",
        display: "flex",
        overflowX: "hidden",
    },
    extendToolbar: {
        ...theme.mixins.toolbar,
    },
}), {index: 1});
