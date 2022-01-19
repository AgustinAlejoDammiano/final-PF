import React, { useState, useEffect } from "react";
import { AppBar, Button, Toolbar, Typography } from "@material-ui/core";
import useStyles from "./styles";
import { Link } from 'react-router-dom';
import { URLS } from "./../../constant/urls";
import UpdateRepository from "../../repository/UpdateRepository";

const logo = "https://www.un.org/sites/un2.un.org/files/covid19_response_icon.svg";
const repository = new UpdateRepository()

export default function Header() {

    const classes = useStyles();

    const [lastUpdate, setLastUpdate] = useState("None")
    
    useEffect(() => {
        repository.getLastUpdate()
            .then((r) => {
                if (r.length !== 0) {
                    setLastUpdate(r.pop().date) //TODO improve
                }
            })
            .catch((error: Error) => console.log(error))
    }, [lastUpdate])

    const onUpdate = () => {
        repository.update()
            .then(() => {window.location.reload();}) //TODO maybe add toast
            .catch((error: Error) => console.log(error))
    }

    return (
        <>
            <AppBar position="fixed" className={classes.appBar}>
                <Toolbar className={classes.toolbar}>
                    <Button className={classes.button} component={Link} to={URLS.home}>
                        <div className={classes.logo}>
                            <img src={logo} alt={'Icon'} className={classes.image} />
                        </div>
                    </Button>
                    <div>
                        <Button className={classes.connectButton} component={Link} to={URLS.dose} >
                            <Typography variant="h4" className={classes.text}>Dose</Typography>
                        </Button>
                        <Button className={classes.connectButton} component={Link} to={URLS.jurisdiction} >
                            <Typography variant="h4" className={classes.text}>Jurisdiction</Typography>
                        </Button>
                        <Button className={classes.connectButton} component={Link} to={URLS.department} >
                            <Typography variant="h4" className={classes.text}>Department</Typography>
                        </Button>
                        <Button className={classes.connectButton} component={Link} to={URLS.vaccine} >
                            <Typography variant="h4" className={classes.text}>Vaccine</Typography>
                        </Button>
                        <Button className={classes.connectButton} onClick={onUpdate} >
                            <Typography variant="h4" className={classes.text}>{`Last update: ${lastUpdate}`}</Typography>
                        </Button>
                    </div>
                </Toolbar>
            </AppBar>
        </>
    );
}
