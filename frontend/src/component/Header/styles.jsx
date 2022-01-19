import { makeStyles } from "@material-ui/styles";
import tinycolor from "tinycolor2";

const white = "#FFFFFF";

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
  connectButton: {
    backgroundColor: theme.palette.primary.main,
    border: "none",
    fontWeight: 500,
    borderRadius: 20,
    marginLeft: "0.25rem",
    marginRight: "0.25rem",
    textTransform: "none",
    "&:hover": {
      border: `1px solid ${tinycolor(white)
        .darken(50)
        .toHexString()}`,
      color: tinycolor(white)
        .darken(50)
        .toHexString()
    },
    "&:focus": {
      border: `1px solid ${tinycolor(white)
        .darken(50)
        .toHexString()}`,
    }
  },
  text: {
    flex: "1 1 auto",
    overflow: "hidden",
    textOverflow: "ellipsis",
    whiteSpace: "nowrap",
    margin: "0 0.5rem 0 0.25rem",
    fontSize: "1rem",
    width: "fit-content",
    fontWeight: 500,
    color: white,
  }
}), { index: 1 });
