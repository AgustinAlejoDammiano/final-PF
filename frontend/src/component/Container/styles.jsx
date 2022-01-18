import { makeStyles } from "@material-ui/styles";

export default makeStyles(theme => ({
    paper: {
        boxShadow: theme.customShadows.widgetDark,
        display: "flex",
        alignItems: "center",
        flexDirection: "column",
        borderRadius: 20,
        maxWidth: "1000px"
    },
    titleRow: {
        justifyContent: 'space-between',
        display: 'flex',
        width: "100%",
        padding: "1rem",
    },
    button: {
        textTransform: "none",
        float: "left",
        padding: "0",
        justifyContent: "flex-start",
        '&:hover': {},
    },
    iconButton: {
        float: "right",
        padding: "0",
    },
    arrow: {
        color: "#000000",
    },
    tooltip: {
        backgroundColor: "#000000",
    },
    container: {
        display: "flex",
        alignItems: "center",  
    }
}), { index: 1 });