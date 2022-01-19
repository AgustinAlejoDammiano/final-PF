import { makeStyles } from "@material-ui/styles";

export default makeStyles(theme => ({
    rowContainer: {
        display: "flex",
        flexDirection: "column",
        justifyContent: "center",
        padding: "1.1rem",
        "& .MuiOutlinedInput-root": {
            borderRadius: 15,
        },
        "& .MuiOutlinedInput-notchedOutline": {
            borderRadius: 15,
        },
    },
    buttonPublication: {
        textTransform: "none",
        color: theme.palette.primary.main,
        borderRadius: 20,
        border: `1px solid ${theme.palette.primary.main}`,
        "&:hover": {
            backgroundColor: "none",
            textDecoration: "underline"
        }
    },
    button: {
        textTransform: "none",
        color: "#FFFFFF",
        backgroundColor: theme.palette.primary.main,
        borderRadius: 15,
        border: `1px solid ${theme.palette.primary.main}`,
        width: "100%",
        "&:hover": {
            backgroundColor: theme.palette.primary.light
        },
        "&.MuiButton-root.Mui-disabled": {
            backgroundColor: "#FFFFFF",
            border: "none"
        }
    },
    formControl: {
        margin: theme.spacing(1),
        minWidth: 820,
    },
    error: {
        color: "#ff0033"
    },
    buttonProgress: {
        color:  theme.palette.primary.light,
        position: 'absolute',
        top: '50%',
        left: '50%',
        marginTop: -12,
        marginLeft: -12,
    },
    rowRuleContainer: {
        display: "flex",
        flexDirection: "row",
        padding: "1.1rem",
        "& .MuiOutlinedInput-root": {
            borderRadius: 15,
        },
        "& .MuiOutlinedInput-notchedOutline": {
            borderRadius: 15,
        },
    },
    ruleLabel: {
        fontWeight: 'bold'
    }
}), {index: 1});
