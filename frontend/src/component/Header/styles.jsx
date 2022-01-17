import { makeStyles } from "@material-ui/styles";

export default makeStyles(theme => ({
  appBar: {
    zIndex: theme.zIndex.drawer + 1,
    transition: theme.transitions.create(["margin"], {
      easing: theme.transitions.easing.sharp,
      duration: theme.transitions.duration.leavingScreen,
    }),
  },
  toolbar: {
    justifyContent: 'space-between',
    paddingLeft: theme.spacing(2),
    paddingRight: theme.spacing(2),
    display: 'flex',
  },
  logo: {
    flexDirection: 'row',
    alignItems: 'center',
    display: 'flex',
    justifyContent: 'center',
  },
  logotype: {
    color: "white",
    fontSize: 18,
    fontWeight: 500,
    marginRight: theme.spacing(2.5),
    marginLeft: theme.spacing(2.5),
    whiteSpace: "nowrap",
    [theme.breakpoints.down("xs")]: {
      display: "none",
    },
  },
  button: {
    textTransform: "none",
  },
  image: {
    height: "50px",
    width: "50px",
    marginRight: "0.5rem"
  },
}), { index: 1 });
