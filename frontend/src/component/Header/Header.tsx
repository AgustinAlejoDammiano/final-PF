import React from "react";
import { AppBar, Button, Toolbar } from "@material-ui/core";
import useStyles from "./styles";
import { Link } from 'react-router-dom';
import { URLS } from "./../../constant/urls";

const logo = "https://www.un.org/sites/un2.un.org/files/covid19_response_icon.svg";

export default function Header() {

    const classes = useStyles();

    return (
        <>
            <AppBar position="fixed" className={classes.appBar}>
                <Toolbar className={classes.toolbar}>
                    <Button className={classes.button} component={Link} to={URLS.home}>
                        <div className={classes.logo}>
                            <img src={logo} alt={'Icon'} className={classes.image} />
                        </div>
                    </Button>
                    {/* TODO */}
                </Toolbar>
            </AppBar>
        </>
    );
}
