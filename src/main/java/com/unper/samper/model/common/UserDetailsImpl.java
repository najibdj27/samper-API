package com.unper.samper.model.common;


import com.fasterxml.jackson.annotation.JsonIgnore;
import com.unper.samper.model.User;
import com.unper.samper.model.constant.EUserStatus;

import lombok.*;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class UserDetailsImpl implements UserDetails {
    private static final long serialVersionUID = 1L;

    private Long id;

    private String firstName;

    private String lastName;

    private String username;

    private String email;

    private String phoneNumber;

    @JsonIgnore
    private String password;
    
    private String faceToken;

    private String registeredFaceUrl;

    private EUserStatus status;
    
    private Boolean isDeleted;

    public Collection<? extends GrantedAuthority> authorities;

    public static UserDetailsImpl build(User user) {
        List<GrantedAuthority> authorities = user.getRoles().stream()
                .map(role -> new SimpleGrantedAuthority(role.getName().name()))
                .collect(Collectors.toList());
        return new UserDetailsImpl(user.getId(), user.getFirstName(), user.getLastName(), user.getUsername(), user.getEmail(), user.getPhoneNumber(), user.getPassword(), user.getFaceToken(), user.getRegisteredFaceUrl(), user.getStatus(), user.getIsDeleted(), authorities);
    }

    @Override
    public boolean isAccountNonExpired() {
        return true;
    }

    @Override
    public boolean isAccountNonLocked() {
        return true;
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return true;
    }

    @Override
    public boolean isEnabled() {
        return true;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        UserDetailsImpl user = (UserDetailsImpl) o;
        return Objects.equals(id, user.id);
    }

    public UserDetailsImpl(Long id2, String firstName2, String lastName2, String username2, String email2,
            Integer phoneNumber2, String password2, List<GrantedAuthority> authorities2) {
    }
}
